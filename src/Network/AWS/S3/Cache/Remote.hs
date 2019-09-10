{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Network.AWS.S3.Cache.Remote
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
module Network.AWS.S3.Cache.Remote where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception.Safe
import Control.Lens
import Control.Monad.Logger as L
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource (MonadResource, ResourceT)
import Crypto.Hash (Digest, HashAlgorithm, digestFromByteString)
import Data.ByteArray as BA
import Data.ByteString as S
import Data.ByteString.Base64 as S64
import Data.Conduit
import Data.Conduit.List as CL
import Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Encoding.Error as T
import Data.Time
import Data.Word (Word64)
import Network.AWS
import Network.AWS.Data.Body
import Network.AWS.Data.Text (toText)
import Network.AWS.S3.Cache.Types
import Network.AWS.S3.CreateMultipartUpload
import Network.AWS.S3.DeleteObject
import Network.AWS.S3.GetObject
import Network.AWS.S3.StreamingUpload
import Network.AWS.S3.Types
import Network.HTTP.Client
import Network.HTTP.Types.Status (Status(statusMessage), status404)
import Prelude as P
import System.IO (hClose)

-- | Returns the time when the cache object was created
getCreateTime :: GetObjectResponse -> Maybe UTCTime
getCreateTime resp =
  (HM.lookup metaCreateTimeKey (resp ^. gorsMetadata) >>= parseISO8601) <|>
  (resp ^. gorsLastModified)

-- | Will check if there is already cache up on AWS and checks if it's contents has changed.
-- Returns create time date if new cache should be uploaded.
hasCacheChanged ::
     ( MonadReader r m
     , MonadResource m
     , MonadLogger m
     , MonadThrow m
     , HasBucketName r BucketName
     , HasObjectKey r ObjectKey
     , HasNumRetries r Int
     , HasEnv r
     , HasMinLogLevel r L.LogLevel
     , HashAlgorithm h
     , Typeable h
     )
  => Digest h
  -> m (Maybe UTCTime)
hasCacheChanged newHash = do
  c <- ask
  let getObjReq = getObject (c ^. bucketName) (c ^. objectKey)
      hashKey = getHashMetaKey newHash
      onErr status
        | status == status404 = do
          logAWS LevelInfo "No previously stored cache was found."
          return (Nothing, (Nothing, Nothing))
        | otherwise = return (Just LevelError, (Nothing, Nothing))
      onSucc resp = do
        logAWS LevelDebug "Discovered previous cache."
        let mOldHash = HM.lookup hashKey (resp ^. gorsMetadata)
            mCreateTime = getCreateTime resp
        case mOldHash of
          Just oldHash ->
            logAWS LevelDebug $ "Hash value for previous cache is " <> hashKey <> ": " <> oldHash
          Nothing ->
            logAWS LevelWarn $ "Previous cache is missing a hash value '" <> hashKey <> "'"
        return (mOldHash, mCreateTime)
  (mOldHashTxt, mCreateTime) <- sendAWS getObjReq onErr onSucc
  createTime <- maybe (liftIO getCurrentTime) return mCreateTime
  mOldHash <- maybe (return Nothing) decodeHash mOldHashTxt
  return
    (if Just newHash /= mOldHash
       then Just createTime
       else Nothing)


encodeHash :: HashAlgorithm h => Digest h -> Text
encodeHash hash = T.decodeUtf8 (S64.encode (BA.convert hash))


decodeHash ::
     (MonadReader c m, HashAlgorithm h, MonadLogger m, HasObjectKey c ObjectKey)
  => Text
  -> m (Maybe (Digest h))
decodeHash hashTxt =
  case S64.decode (T.encodeUtf8 hashTxt) of
    Left err -> do
      logAWS LevelError $
        "Problem decoding cache's hash value: " <> hashTxt <> " Decoding Error: " <> T.pack err
      return Nothing
    Right bstr -> return $ digestFromByteString bstr


uploadCache ::
     ( MonadReader r m
     , HasEnv r
     , HasMinLogLevel r L.LogLevel
     , HasObjectKey r ObjectKey
     , HasBucketName r BucketName
     , HasMaxSize r (Maybe Integer)
     , HasNumRetries r Int
     , MonadResource m
     , MonadLoggerIO m
     , MonadThrow m
     , HashAlgorithm h
     , Typeable h
     )
  => Bool
  -> TempFile -- ^ Temporary file where cache has been written to
  -> (Word64, Digest h) -- ^ Size and hash of the temporary file with cache
  -> MaybeT m ()
uploadCache isPublic tmpFile (cSize, newHash) = do
  c <- ask
  when (maybe False (fromIntegral cSize >=) (c ^. maxSize)) $ do
    logAWS LevelInfo $
      "Refusing to save, cache is too big: " <> formatBytes (fromIntegral cSize)
    MaybeT $ return Nothing
  mCreatedTime <- hasCacheChanged newHash
  createTime <- mCreatedTime `onNothing` logAWS LevelInfo "No change to cache was detected."
  let newHashTxt = encodeHash newHash
      hashKey = getHashMetaKey newHash
      cmu =
        createMultipartUpload (c ^. bucketName) (c ^. objectKey) & cmuMetadata .~
        HM.fromList
          [ (metaHashAlgorithmKey, hashKey)
          , (metaCreateTimeKey, formatISO8601 createTime)
          , (hashKey, newHashTxt)
          , (compressionMetaKey, getCompressionName (tempFileCompression tmpFile))
          ] &
        if isPublic
          then cmuACL ?~ OPublicRead
          else id
  logAWS LevelInfo $
    "Data change detected, caching " <> formatBytes (fromIntegral cSize) <> " with " <> hashKey <>
    ": " <>
    newHashTxt
  startTime <- liftIO getCurrentTime
  liftIO $ hClose (tempFileHandle tmpFile)
  runLoggingAWS_ $
    void $
    concurrentUpload
      (Just (8 * 1024 ^ (2 :: Int)))
      (Just 10)
      (FP (tempFilePath tmpFile))
      cmu
  endTime <- liftIO getCurrentTime
  reportSpeed cSize $ diffUTCTime endTime startTime
  logAWS LevelInfo "Finished uploading. Files are cached on S3."

reportSpeed ::
     (MonadReader a m, MonadLogger m, HasObjectKey a ObjectKey, Real p1, Real p2)
  => p1
  -> p2
  -> m ()
reportSpeed cSize delta = logAWS LevelInfo $ "Average speed: " <> formatBytes speed <> "/s"
  where
    speed
      | delta == 0 = 0
      | otherwise = round (toRational cSize / toRational delta)


onNothing :: Monad m => Maybe b -> m a -> MaybeT m b
onNothing mArg whenNothing =
  case mArg of
    Nothing -> do
      _ <- lift whenNothing
      MaybeT $ return Nothing
    Just res -> MaybeT $ return $ Just res



deleteCache ::
     ( MonadResource m
     , MonadLoggerIO m
     , MonadReader c m
     , MonadThrow m
     , HasEnv c
     , HasMinLogLevel c L.LogLevel
     , HasNumRetries c Int
     , HasObjectKey c ObjectKey
     , HasBucketName c BucketName
     )
  => m ()
deleteCache = do
  c <- ask
  sendAWS_
    (deleteObject (c ^. bucketName) (c ^. objectKey))
    (const $ logAWS LevelInfo "Clear cache request was successfully submitted.")

-- | Download an object from S3 and handle its content using the supplied sink.
downloadCache ::
     ( MonadResource m
     , MonadLoggerIO m
     , MonadReader c m
     , MonadThrow m
     , HasEnv c
     , HasMinLogLevel c L.LogLevel
     , HasObjectKey c ObjectKey
     , HasBucketName c BucketName
     , HasMaxAge c (Maybe NominalDiffTime)
     , HasMaxSize c (Maybe Integer)
     , HasNumRetries c Int
     )
  => (forall h . HashAlgorithm h =>
       (L.LogLevel -> Text -> IO ())
       -> Compression
       -> h
       -> ConduitT S.ByteString Void (ResourceT IO) (Digest h))
  -> MaybeT m ()
downloadCache sink = do
  c <- ask
  let getObjReq = getObject (c ^. bucketName) (c ^. objectKey)
      onErr status
        | status == status404 = do
          logAWS LevelInfo "No previously stored cache was found."
          MaybeT $ return Nothing
        | otherwise = pure (Just LevelError, ())
  logAWS LevelDebug "Checking for previously stored cache."
  sendAWS getObjReq onErr $ \resp -> do
    logAWS LevelDebug "Starting to download previous cache."
    compAlgTxt <-
      HM.lookup compressionMetaKey (resp ^. gorsMetadata) `onNothing`
      logAWS LevelWarn "Missing information on compression algorithm."
    compAlg <-
      readCompression compAlgTxt `onNothing`
      logAWS LevelWarn ("Compression algorithm is not supported: " <> compAlgTxt)
    logAWS LevelDebug $ "Compression algorithm used: " <> compAlgTxt
    hashAlgName <-
      HM.lookup metaHashAlgorithmKey (resp ^. gorsMetadata) `onNothing`
      logAWS LevelWarn "Missing information on hashing algorithm."
    logAWS LevelDebug $ "Hashing algorithm used: " <> hashAlgName
    hashTxt <-
      HM.lookup hashAlgName (resp ^. gorsMetadata) `onNothing`
      logAWS LevelWarn ("Cache is missing a hash value '" <> hashAlgName <> "'")
    logAWS LevelDebug $ "Hash value is " <> hashAlgName <> ": " <> hashTxt
    createTime <- getCreateTime resp `onNothing`
      logAWS LevelWarn "Cache is missing creation time info."
    logAWS LevelDebug $ "Cache creation timestamp:  " <> formatRFC822 createTime
    case c ^. maxAge of
      Nothing -> return ()
      Just timeDelta -> do
        curTime <- liftIO getCurrentTime
        when (curTime >= addUTCTime timeDelta createTime) $ do
          logAWS LevelInfo $
            "Refusing to restore, cache is too old: " <>
            formatDiffTime (diffUTCTime curTime createTime)
          deleteCache
          MaybeT $ return Nothing
    case (,) <$> (resp ^. gorsContentLength) <*> (c ^. maxSize) of
      Nothing -> return ()
      Just (len, maxLen) ->
        when (len >= maxLen) $ do
          logAWS LevelInfo $ "Refusing to restore, cache is too big: " <> formatBytes len
          deleteCache
          MaybeT $ return Nothing
    let noHashAlgSupport _ =
          logAWS LevelWarn $ "Hash algorithm used for the cache is not supported: " <> hashAlgName
    withHashAlgorithm_ hashAlgName noHashAlgSupport $ \hashAlg -> do
      mHashExpected <- decodeHash hashTxt
      hashExpected <-
        mHashExpected `onNothing`
        logAWS LevelError ("Problem decoding cache's hash value: " <> hashTxt)
      len <-
        (resp ^. gorsContentLength) `onNothing`
        logAWS LevelError "Did not receive expected cache size form AWS"
      logAWS LevelInfo $
        "Restoring cache from " <>
        formatRFC822 (fromMaybe createTime (resp ^. gorsLastModified)) <>
        " with total size: " <> formatBytes len
      logger <- getLoggerIO
      hashComputed <-
        liftIO $
        runConduitRes (
        (resp ^. gorsBody ^. to _streamBody) .|
        getProgressReporter (logger LevelInfo) (fromInteger len) .| sink logger compAlg hashAlg)
      if hashComputed == hashExpected
        then
          logAWS LevelInfo $
            "Successfully restored previous cache with hash: " <> encodeHash hashComputed
        else do
          logAWS LevelError $
            "Computed '" <> hashAlgName <> "' hash mismatch: '" <> encodeHash hashComputed <>
            "' /= '" <>
            encodeHash hashExpected <>
            "'"
          MaybeT $ return Nothing


-- | Send request to AWS and process the response with a handler. A separate error handler will be
-- invoked whenever an error occurs, which suppose to return some sort of default value and the
-- `L.LogLevel` this error corresponds to.
sendAWS ::
     ( MonadReader r m
     , MonadResource m
     , HasEnv r
     , HasMinLogLevel r L.LogLevel
     , HasObjectKey r ObjectKey
     , HasNumRetries r Int
     , AWSRequest a
     , MonadLogger m
     , MonadThrow m
     )
  => a
  -> (Status -> m (Maybe L.LogLevel, b))
  -> (Rs a -> m b)
  -> m b
sendAWS req = runLoggingAWS (send req)


-- | Same as `sendAWS`, but discard the response and simply error out on any received AWS error
-- responses.
sendAWS_ ::
     ( MonadReader r m
     , MonadResource m
     , HasEnv r
     , HasMinLogLevel r L.LogLevel
     , HasNumRetries r Int
     , HasObjectKey r ObjectKey
     , AWSRequest a
     , MonadLogger m
     , MonadThrow m
     )
  => a
  -> (Rs a -> m ())
  -> m ()
sendAWS_ req = runLoggingAWS (send req) (const $ return (Just LevelError, ()))


-- | Report every problem as `LevelError` and discard the result.
runLoggingAWS_ ::
     ( MonadReader r m
     , MonadResource m
     , HasEnv r
     , HasMinLogLevel r L.LogLevel
     , HasNumRetries r Int
     , HasObjectKey r ObjectKey
     , MonadLogger m
     , MonadThrow m
     )
  => AWS ()
  -> m ()
runLoggingAWS_ action = runLoggingAWS action (const $ pure (Just LevelError, ())) return


-- | General helper for calling AWS and conditionally log the outcome upon a received error.
runLoggingAWS ::
     ( MonadReader r m
     , MonadResource m
     , HasEnv r
     , HasMinLogLevel r L.LogLevel
     , HasNumRetries r Int
     , HasObjectKey r ObjectKey
     , MonadLogger m
     , MonadThrow m
     )
  => AWS t
  -> (Status -> m (Maybe L.LogLevel, b))
  -> (t -> m b)
  -> m b
runLoggingAWS action onErr onSucc = do
  conf <- ask
  eResp <- retryWith (runAWS conf $ trying _Error action)
  case eResp of
    Left err -> do
      (errMsg, status) <-
        case err of
          TransportError exc -> do
            unless ((conf ^. minLogLevel) == L.LevelDebug) $
                logAWS LevelError $ "Critical HTTPException: " <> toErrorMessage exc
            throwM exc
          SerializeError serr -> return (T.pack (serr ^. serializeMessage), serr ^. serializeStatus)
          ServiceError serr -> do
            let status = serr ^. serviceStatus
                errMsg =
                  maybe
                    (T.decodeUtf8With T.lenientDecode $ statusMessage status)
                    toText
                    (serr ^. serviceMessage)
            return (errMsg, status)
      (mLevel, def) <- onErr status
      case mLevel of
        Just level -> logAWS level errMsg
        Nothing -> return ()
      return def
    Right suc -> onSucc suc

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Convert an HTTP exception into a readable error message
toErrorMessage :: HttpException -> Text
toErrorMessage exc =
  case exc of
    HttpExceptionRequest _ httpExcContent ->
      case httpExcContent of
        StatusCodeException resp _ -> "StatusCodeException: " <> tshow (responseStatus resp)
        TooManyRedirects rs -> "TooManyRedirects: " <> tshow (P.length rs)
        InvalidHeader _ -> "InvalidHeader"
        InvalidRequestHeader _ -> "InvalidRequestHeader"
        InvalidProxyEnvironmentVariable name _ -> "InvalidProxyEnvironmentVariable: " <> name
        _ -> tshow httpExcContent
    _ -> T.pack (displayException exc)

-- | Retry the provided action
retryWith ::
     ( HasNumRetries r Int
     , HasObjectKey r ObjectKey
     , MonadIO m
     , MonadLogger m
     , MonadReader r m
     )
  => m (Either Error a)
  -> m (Either Error a)
retryWith action = do
  conf <- ask
  let n = conf ^. numRetries
      go i eResp =
        case eResp of
          Left (TransportError exc)
            | i > n -> pure eResp
            | otherwise -> do
              let s = min 9 (i * i) -- exponential backoff with at most 9 seconds
              logAWS LevelWarn $ "TransportError - " <> toErrorMessage exc
              logAWS LevelWarn $
                "Retry " <> tshow i <> "/" <> tshow n <> ". Waiting for " <> tshow s <> " seconds"
              liftIO $ threadDelay (s * 1000000) -- microseconds
              eResp' <- action
              go (i + 1) eResp'
          _ -> pure eResp
  action >>= go 1

-- | Logger that will add object info to the entry.
logAWS :: (MonadReader a m, MonadLogger m, HasObjectKey a ObjectKey) =>
          L.LogLevel -> Text -> m ()
logAWS ll msg = do
  c <- ask
  let ObjectKey objKeyTxt = c ^. objectKey
  logOtherN ll $ "<" <> objKeyTxt <> "> - " <> msg


-- | Compute chunk thresholds and report progress.
reportProgress ::
     (MonadIO m)
  => (Word64 -> Word64 -> m a)
  -> ([(Word64, Word64)], Word64, Word64, UTCTime)
  -> Word64
  -> m ([(Word64, Word64)], Word64, Word64, UTCTime)
reportProgress reporter (thresh, stepSum, prevSum, prevTime) chunkSize
  | P.null thresh || curSum < curThresh = return (thresh, stepSum + chunkSize, curSum, prevTime)
  | otherwise = do
    curTime <- liftIO getCurrentTime
    let delta = diffUTCTime curTime prevTime
        speed = if delta == 0
                then 0
                else (toInteger (stepSum + chunkSize) % 1) / toRational delta
    _ <- reporter perc $ round (fromRational @Double speed)
    return (restThresh, 0, curSum, curTime)
  where
    curSum = prevSum + chunkSize
    (perc, curThresh):restThresh = thresh


-- | Helper action that returns a function that can do info level logging in the `MonadIO`.
getLoggerIO :: (MonadLoggerIO m) => m (L.LogLevel -> Text -> IO ())
getLoggerIO = do
  loggerIO <- askLoggerIO
  return $ \ level txt -> loggerIO defaultLoc "" level (toLogStr txt)


-- | Creates a conduit that will execute supplied action 10 time each for every 10% of the data is
-- being passed through it. Supplied action will receive `Text` with status and speed of processing.
getProgressReporter ::
     MonadIO m => (Text -> IO ()) -> Word64 -> ConduitM S.ByteString S.ByteString m ()
getProgressReporter reporterTxt totalSize = do
  let thresh = [(p, (totalSize * p) `div` 100) | p <- [10,20 .. 100]]
      reporter perc speed =
        liftIO $ reporterTxt $
        "Progress: " <> T.pack (show perc) <> "%, speed: " <> formatBytes (fromIntegral speed) <>
        "/s"
      reportProgressAccum chunk acc = do
        acc' <- reportProgress reporter acc (fromIntegral (S.length chunk))
        return (acc', chunk)
  curTime <- liftIO getCurrentTime
  void $ CL.mapAccumM reportProgressAccum (thresh, 0, 0, curTime)
