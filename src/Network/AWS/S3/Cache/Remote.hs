{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Control.Lens hiding ((^.), to)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource (liftResourceT)
import Crypto.Hash (Digest, HashAlgorithm, digestFromByteString)
import Data.ByteArray as BA
import Data.ByteString as S
import Data.ByteString.Base64 as S64
import Data.Conduit
import Data.Conduit.List as CL
import RIO.HashMap as HM
import Data.Ratio ((%))
import RIO.Text as T
import RIO.Time
import Network.AWS hiding (LogLevel)
import Network.AWS.Data.Body
import Network.AWS.Data.Text (toText)
import Network.AWS.S3.Cache.Types
import Network.AWS.S3.Cache.Local (restoreFilesFromCache)
import Network.AWS.S3.CreateMultipartUpload
import Network.AWS.S3.DeleteObject
import Network.AWS.S3.GetObject
import Network.AWS.S3.StreamingUpload
import Network.AWS.S3.Types
import Network.HTTP.Client
import Network.HTTP.Types.Status (Status(statusMessage), status404)
import RIO
import RIO.List as L

import Data.Conduit.Binary


-- | Returns the time when the cache object was created
getCreateTime :: GetObjectResponse -> Maybe UTCTime
getCreateTime resp =
  (HM.lookup metaCreateTimeKey (resp ^. gorsMetadata) >>= parseISO8601) <|>
  (resp ^. gorsLastModified)

-- | Will check if there is already cache up on AWS and checks if it's contents has changed.
-- Returns create time date if new cache should be uploaded.
hasCacheChanged ::
     ( MonadReader r m
     , MonadIO m
     , MonadThrow m
     , HasBucketName r BucketName
     , HasLogFunc r
     , HasObjectKey r ObjectKey
     , HasNumRetries r Int
     , HasEnv r
     , HasMinLogLevel r LogLevel
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
            logAWS LevelDebug $
            "Hash value for previous cache is " <> display hashKey <> ": " <> display oldHash
          Nothing ->
            logAWS LevelWarn $ "Previous cache is missing a hash value '" <> display hashKey <> "'"
        return (mOldHash, mCreateTime)
  (mOldHashTxt, mCreateTime) <- sendAWS getObjReq onErr onSucc
  createTime <- maybe getCurrentTime return mCreateTime
  mOldHash <- maybe (return Nothing) decodeHash mOldHashTxt
  return
    (if Just newHash /= mOldHash
       then Just createTime
       else Nothing)


encodeHash :: HashAlgorithm h => Digest h -> Utf8Builder
encodeHash hash = displayBytesUtf8 (S64.encode (BA.convert hash))


decodeHash ::
     (MonadIO m, MonadReader env m, HashAlgorithm h, HasLogFunc env, HasObjectKey env ObjectKey)
  => Text
  -> m (Maybe (Digest h))
decodeHash hashTxt =
  case S64.decode (T.encodeUtf8 hashTxt) of
    Left err -> do
      logAWS LevelError $
        "Problem decoding cache's hash value: " <> display hashTxt <> " Decoding Error: " <>
        fromString err
      return Nothing
    Right bstr -> return $ digestFromByteString bstr


uploadCache ::
     ( MonadReader r m
     , MonadIO m
     , HasEnv r
     , HasMinLogLevel r LogLevel
     , HasObjectKey r ObjectKey
     , HasBucketName r BucketName
     , HasLogFunc r
     , HasMaxSize r (Maybe Integer)
     , HasNumRetries r Int
     , MonadThrow m
     , HashAlgorithm h
     , Typeable h
     )
  => Bool
  -> TempFile -- ^ Temporary file where cache has been written to
  -> (Word64, Digest h) -- ^ Size and hash of the temporary file with cache
  -> m ()
uploadCache isPublic tmpFile (cSize, newHash) =
  void $
  runMaybeT $ do
    c <- ask
    when (maybe False (fromIntegral cSize >=) (c ^. maxSize)) $ do
      logAWS LevelInfo $ "Refusing to save, cache is too big: " <> formatBytes (fromIntegral cSize)
      MaybeT $ return Nothing
    mCreatedTime <- hasCacheChanged newHash
    createTime <- mCreatedTime `onNothing` logAWS LevelInfo "No change to cache was detected."
    let newHashTxt = textDisplay $ encodeHash newHash
        hashKey = getHashMetaKey newHash
        cmu =
          createMultipartUpload (c ^. bucketName) (c ^. objectKey) &
          cmuMetadata .~
          HM.fromList
            [ (metaHashAlgorithmKey, hashKey)
            , (metaCreateTimeKey, textDisplay $ formatISO8601 createTime)
            , (hashKey, newHashTxt)
            , (compressionMetaKey, getCompressionName (tempFileCompression tmpFile))
            ] &
          if isPublic
            then cmuACL ?~ OPublicRead
            else id
    logAWS LevelInfo $
      mconcat
        [ "Data change detected, caching "
        , formatBytes (fromIntegral cSize)
        , " with "
        , display hashKey
        , ": "
        , display newHashTxt
        ]
    startTime <- getCurrentTime
    runLoggingAWS_ $
      runConduit $
      sourceHandle (tempFileHandle tmpFile) .|
      passthroughSink (streamUpload (Just (100 * 2 ^ (20 :: Int))) cmu) (void . pure) .|
      transPipe (runRIO c) (getProgressReporter cSize) .|
      sinkNull
    -- Disabled due to: https://github.com/fpco/cache-s3/issues/26
    -- hClose (tempFileHandle tmpFile)
    -- runLoggingAWS_ $
    --   void $
    --   concurrentUpload
    --     (Just (8 * 1024 ^ (2 :: Int)))
    --     (Just 10)
    --     (FP (tempFilePath tmpFile))
    --     cmu
    endTime <- getCurrentTime
    reportSpeed cSize $ diffUTCTime endTime startTime
    logAWS LevelInfo "Finished uploading. Files are cached on S3."

reportSpeed ::
     (MonadIO m, MonadReader env m, HasLogFunc env, HasObjectKey env ObjectKey, Real p1, Real p2)
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
     ( MonadReader c m
     , MonadThrow m
     , MonadIO m
     , HasEnv c
     , HasLogFunc c
     , HasMinLogLevel c LogLevel
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
restoreCache ::
     (MonadIO m, MonadReader Config m, MonadThrow m, PrimMonad m, MonadUnliftIO m)
  => FileOverwrite
  -> m Bool
restoreCache fileOverwrite =
  fmap isJust $
  runMaybeT $ do
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
        logAWS LevelWarn ("Compression algorithm is not supported: " <> display compAlgTxt)
      logAWS LevelDebug $ "Compression algorithm used: " <> display compAlgTxt
      hashAlgName <-
        HM.lookup metaHashAlgorithmKey (resp ^. gorsMetadata) `onNothing`
        logAWS LevelWarn "Missing information on hashing algorithm."
      logAWS LevelDebug $ "Hashing algorithm used: " <> display hashAlgName
      hashTxt <-
        HM.lookup hashAlgName (resp ^. gorsMetadata) `onNothing`
        logAWS LevelWarn ("Cache is missing a hash value '" <> display hashAlgName <> "'")
      logAWS LevelDebug $ "Hash value is " <> display hashAlgName <> ": " <> display hashTxt
      createTime <-
        getCreateTime resp `onNothing` logAWS LevelWarn "Cache is missing creation time info."
      logAWS LevelDebug $ "Cache creation timestamp:  " <> formatRFC822 createTime
      case c ^. maxAge of
        Nothing -> return ()
        Just timeDelta -> do
          curTime <- getCurrentTime
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
            logAWS LevelWarn $
            "Hash algorithm used for the cache is not supported: " <> display hashAlgName
      withHashAlgorithm_ hashAlgName noHashAlgSupport $ \hashAlg -> do
        mHashExpected <- decodeHash hashTxt
        hashExpected <-
          mHashExpected `onNothing`
          logAWS LevelError ("Problem decoding cache's hash value: " <> display hashTxt)
        len <-
          (resp ^. gorsContentLength) `onNothing`
          logAWS LevelError "Did not receive expected cache size form AWS"
        logAWS LevelInfo $
          "Restoring cache from " <> formatRFC822 (fromMaybe createTime (resp ^. gorsLastModified)) <>
          " with total size: " <>
          formatBytes len
        hashComputed <-
          lift $
          runConduitRes $
          transPipe liftResourceT (resp ^. gorsBody ^. to _streamBody) .|
          getProgressReporter (fromInteger len) .|
          restoreFilesFromCache fileOverwrite compAlg hashAlg
        if hashComputed == hashExpected
          then logAWS LevelInfo $
               "Successfully restored previous cache with hash: " <> encodeHash hashComputed
          else do
            logAWS LevelError $
              mconcat
                [ "Computed '"
                , display hashAlgName
                , "' hash mismatch: '"
                , encodeHash hashComputed
                , "' /= '"
                , encodeHash hashExpected
                , "'"
                ]
            MaybeT $ return Nothing


-- | Send request to AWS and process the response with a handler. A separate error handler will be
-- invoked whenever an error occurs, which suppose to return some sort of default value and the
-- `LogLevel` this error corresponds to.
sendAWS ::
     ( MonadReader r m
     , MonadIO m
     , HasEnv r
     , HasLogFunc r
     , HasMinLogLevel r LogLevel
     , HasObjectKey r ObjectKey
     , HasNumRetries r Int
     , AWSRequest a
     , MonadThrow m
     )
  => a
  -> (Status -> m (Maybe LogLevel, b))
  -> (Rs a -> m b)
  -> m b
sendAWS req = runLoggingAWS (send req)


-- | Same as `sendAWS`, but discard the response and simply error out on any received AWS error
-- responses.
sendAWS_ ::
     ( MonadReader r m
     , MonadIO m
     , HasEnv r
     , HasLogFunc r
     , HasMinLogLevel r LogLevel
     , HasNumRetries r Int
     , HasObjectKey r ObjectKey
     , AWSRequest a
     , MonadThrow m
     )
  => a
  -> (Rs a -> m ())
  -> m ()
sendAWS_ req = sendAWS req (const $ pure (Just LevelError, ()))


-- | Report every problem as `LevelError` and discard the result.
runLoggingAWS_ ::
     ( MonadReader r m
     , MonadIO m
     , HasEnv r
     , HasLogFunc r
     , HasMinLogLevel r LogLevel
     , HasNumRetries r Int
     , HasObjectKey r ObjectKey
     , MonadThrow m
     )
  => AWS ()
  -> m ()
runLoggingAWS_ action = runLoggingAWS action (const $ pure (Just LevelError, ())) return


-- | General helper for calling AWS and conditionally log the outcome upon a received error.
runLoggingAWS ::
     ( MonadReader r m
     , MonadIO m
     , HasEnv r
     , HasLogFunc r
     , HasMinLogLevel r LogLevel
     , HasNumRetries r Int
     , HasObjectKey r ObjectKey
     , MonadThrow m
     )
  => AWS t
  -> (Status -> m (Maybe LogLevel, b))
  -> (t -> m b)
  -> m b
runLoggingAWS action onErr onSucc = do
  conf <- ask
  eResp <- retryWith (liftIO $ runResourceT $ runAWS conf $ trying _Error action)
  case eResp of
    Left err -> do
      (errMsg, status) <-
        case err of
          TransportError exc -> do
            unless ((conf ^. minLogLevel) == LevelDebug) $
              logAWS LevelError $ "Critical HTTPException: " <> toErrorMessage exc
            throwM exc
          SerializeError serr ->
            return (fromString (serr ^. serializeMessage), serr ^. serializeStatus)
          ServiceError serr -> do
            let status = serr ^. serviceStatus
                errMsg =
                  display $
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

-- | Convert an HTTP exception into a readable error message
toErrorMessage :: HttpException -> Utf8Builder
toErrorMessage exc =
  case exc of
    HttpExceptionRequest _ httpExcContent ->
      case httpExcContent of
        StatusCodeException resp _ -> "StatusCodeException: " <> displayShow (responseStatus resp)
        TooManyRedirects rs -> "TooManyRedirects: " <> display (L.length rs)
        InvalidHeader _ -> "InvalidHeader"
        InvalidRequestHeader _ -> "InvalidRequestHeader"
        InvalidProxyEnvironmentVariable name _ ->
          "InvalidProxyEnvironmentVariable: " <> display name
        _ -> displayShow httpExcContent
    _ -> fromString (displayException exc)

-- | Retry the provided action
retryWith ::
     ( HasNumRetries r Int
     , HasLogFunc r
     , HasObjectKey r ObjectKey
     , MonadIO m
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
                "Retry " <> display i <> "/" <> display n <> ". Waiting for " <> display s <>
                " seconds"
              liftIO $ threadDelay (s * 1000000) -- microseconds
              eResp' <- action
              go (i + 1) eResp'
          _ -> pure eResp
  action >>= go 1

-- | Logger that will add object info to the entry.
logAWS :: (MonadIO m, MonadReader a m, HasLogFunc a, HasObjectKey a ObjectKey) =>
          LogLevel -> Utf8Builder -> m ()
logAWS ll msg = do
  c <- ask
  let ObjectKey objKeyTxt = c ^. objectKey
  logGeneric "" ll $ "<" <> display objKeyTxt <> "> - " <> msg

-- | Compute chunk thresholds and report progress.
reportProgress ::
     (MonadIO m)
  => (Word64 -> Word64 -> m a)
  -> ([(Word64, Word64)], Word64, Word64, UTCTime)
  -> Word64
  -> m ([(Word64, Word64)], Word64, Word64, UTCTime)
reportProgress reporter (thresh, stepSum, prevSum, prevTime) chunkSize
  | L.null thresh || curSum < curThresh = return (thresh, stepSum + chunkSize, curSum, prevTime)
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


-- | Creates a conduit that will execute supplied action 10 time each for every 10% of the data is
-- being passed through it. Supplied action will receive `Text` with status and speed of processing.
getProgressReporter ::
     (MonadIO m, MonadReader r m, HasLogFunc r) => Word64 -> ConduitM S.ByteString S.ByteString m ()
getProgressReporter totalSize = do
  let thresh = [(p, (totalSize * p) `div` 100) | p <- [10,20 .. 100]]
      reporter perc speed =
        logInfo $
        "Progress: " <> display perc <> "%, speed: " <> formatBytes (fromIntegral speed) <>
        "/s"
      reportProgressAccum chunk acc = do
        acc' <- reportProgress reporter acc (fromIntegral (S.length chunk))
        return (acc', chunk)
  curTime <- getCurrentTime
  void $ CL.mapAccumM reportProgressAccum (thresh, 0, 0, curTime)
