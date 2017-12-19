{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
-- |
-- Module      : Network.AWS.S3.Cache.Remote
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
module Network.AWS.S3.Cache.Remote where

import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Logger                 as L
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource         (MonadResource)
import           Control.Monad.Trans.Resource         (ResourceT)
import           Crypto.Hash                          (Digest, HashAlgorithm,
                                                       digestFromByteString)
import           Data.ByteArray                       as BA
import           Data.ByteString                      as S
import           Data.ByteString.Base64               as S64
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List                    as CL
import           Data.HashMap.Strict                  as HM
import           Data.Maybe
import           Data.Monoid                          ((<>))
import           Data.Ratio                           ((%))
import           Data.Text                            as T
import           Data.Text.Encoding                   as T
import           Data.Text.Encoding.Error             as T
import           Data.Time
import           Data.Word                            (Word64)
import           Network.AWS
import           Network.AWS.Data.Body
import           Network.AWS.Data.Text                (toText)
import           Network.AWS.S3.Cache.Types
import           Network.AWS.S3.CreateMultipartUpload
import           Network.AWS.S3.DeleteObject
import           Network.AWS.S3.GetObject
import           Network.AWS.S3.StreamingUpload
import           Network.AWS.S3.Types
import           Network.HTTP.Types.Status            (Status (statusMessage),
                                                       status404)
import           Prelude                              as P
import           System.IO                            (Handle)


hasCacheChanged ::
     ( MonadReader r m
     , MonadResource m
     , MonadLogger m
     , HasBucketName r BucketName
     , HasObjectKey r ObjectKey
     , HasEnv r
     , HasMinLogLevel r L.LogLevel
     , HashAlgorithm h
     , Typeable h
     )
  => Digest h
  -> m Bool
hasCacheChanged newHash = do
  c <- ask
  let getObjReq = getObject (c ^. bucketName) (c ^. objectKey)
      hashKey = getHashMetaKey newHash
      onErr status = do
        case () of
          () | status == status404 -> do
              logAWS LevelInfo "No previously stored cache was found."
              return (Nothing, Nothing)
          _ -> return (Just LevelError, Nothing)
      onSucc resp = do
        logAWS LevelDebug $ "Discovered previous cache."
        let mOldHash = HM.lookup hashKey (resp ^. gorsMetadata)
        case mOldHash of
          Just oldHash ->
            logAWS LevelDebug $ "Hash value for previous cache is " <> hashKey <> ": " <> oldHash
          Nothing -> do
            logAWS LevelWarn $ "Previous cache is missing a hash value '" <> hashKey <> "'"
        return $ mOldHash
  mOldHashTxt <- sendAWS getObjReq onErr onSucc
  mOldHash <- maybe (return Nothing) decodeHash mOldHashTxt
  return (Just newHash /= mOldHash)


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
     , MonadResource m
     , MonadLoggerIO m
     , HashAlgorithm h
     , Typeable h
     )
  => (Handle, Word64, Digest h, Compression)
  -> m ()
uploadCache (hdl, cSize, newHash, comp) = do
  c <- ask
  hasChanged <- hasCacheChanged newHash
  if hasChanged
    then do
      let newHashTxt = encodeHash newHash
          hashKey = getHashMetaKey newHash
          cmu =
            createMultipartUpload (c ^. bucketName) (c ^. objectKey) & cmuMetadata .~
            HM.fromList
              [ (hashAlgorithmMetaKey, hashKey)
              , (hashKey, newHashTxt)
              , (compressionMetaKey, getCompressionName comp)
              ]
      logAWS LevelInfo $
        "Data change detected, caching " <> formatBytes (fromIntegral cSize) <> " with " <> hashKey <>
        ": " <>
        newHashTxt
      reporter <- getInfoLoggerIO
      runLoggingAWS_ $
        runConduit $
        sourceHandle hdl .|
        passthroughSink (streamUpload (Just (100 * 2 ^ (20 :: Int))) cmu) (void . pure) .|
        getProgressReporter reporter cSize .|
        sinkNull
      logAWS LevelInfo $ "Finished uploading. Files are cached on S3."
    else logAWS LevelInfo "No change to cache was detected."




onNothing :: Monad m => Maybe b -> m a -> MaybeT m b
onNothing mArg whenNothing = do
  case mArg of
    Nothing -> do
      _ <- lift whenNothing
      MaybeT $ return Nothing
    Just res -> MaybeT $ return $ Just res



deleteCache ::
     ( MonadResource m
     , MonadLoggerIO m
     , MonadReader c m
     , HasEnv c
     , HasMinLogLevel c L.LogLevel
     , HasObjectKey c ObjectKey
     , HasBucketName c BucketName
     )
  => m ()
deleteCache = do
  c <- ask
  sendAWS_
    (deleteObject (c ^. bucketName) (c ^. objectKey))
    (const $ logAWS LevelInfo $ "Clear cache request was successfully submitted.")

-- | Download an object from S3 and handle its content using the supplied sink.
downloadCache ::
     ( MonadResource m
     , MonadLoggerIO m
     , MonadReader c m
     , HasEnv c
     , HasMinLogLevel c L.LogLevel
     , HasObjectKey c ObjectKey
     , HasBucketName c BucketName
     )
  => (forall h. HashAlgorithm h =>
                  Compression -> h -> Sink S.ByteString (ResourceT IO) (Digest h))
  -> MaybeT m ()
downloadCache sink = do
  c <- ask
  let getObjReq = getObject (c ^. bucketName) (c ^. objectKey)
      onErr status = do
        case () of
          () | status == status404 -> do
              logAWS LevelInfo "No previously stored cache was found."
              MaybeT $ return Nothing
          _ -> return (Just LevelError, ())
  logAWS LevelDebug "Checking for previously stored cache."
  sendAWS getObjReq onErr $ \resp -> do
    logAWS LevelDebug $ "Starting to download previous cache."
    compAlgTxt <-
      HM.lookup compressionMetaKey (resp ^. gorsMetadata) `onNothing`
      logAWS LevelWarn "Missing information on compression algorithm."
    compAlg <-
      readCompression compAlgTxt `onNothing`
      (logAWS LevelWarn $ "Compression algorithm is not supported: " <> compAlgTxt)
    logAWS LevelDebug $ "Compression algorithm used: " <> compAlgTxt
    hashAlgName <-
      HM.lookup hashAlgorithmMetaKey (resp ^. gorsMetadata) `onNothing`
      logAWS LevelWarn "Missing information on hashing algorithm."
    logAWS LevelDebug $ "Hashing algorithm used: " <> hashAlgName
    hashTxt <-
      HM.lookup hashAlgName (resp ^. gorsMetadata) `onNothing`
      (logAWS LevelWarn $ "Cache is missing a hash value '" <> hashAlgName <> "'")
    logAWS LevelDebug $ "Hash value is " <> hashAlgName <> ": " <> hashTxt
    let noHashAlgSupport _ = do
          logAWS LevelWarn $ "Hash algorithm used for the cache is not supported: " <> hashAlgName
    withHashAlgorithm_ hashAlgName noHashAlgSupport $ \hashAlg -> do
      mHashExpected <- decodeHash hashTxt
      hashExpected <-
        mHashExpected `onNothing`
        (logAWS LevelError $ "Problem decoding cache's hash value: " <> hashTxt)
      len <-
        (resp ^. gorsContentLength) `onNothing`
        logAWS LevelError "Did not receive expected cache size form AWS"
      logAWS LevelInfo $ "Restoring cache with total size: " <> formatBytes len
      reporter <- getInfoLoggerIO
      hashComputed <-
        liftIO $
        runResourceT $
        resp ^. gorsBody ^. to _streamBody $$+-
        (getProgressReporter reporter (fromInteger len) .| sink compAlg hashAlg)
      if (hashComputed == hashExpected)
        then do
          logAWS LevelInfo "Successfully restored previous cache"
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
     , AWSRequest a
     , MonadLogger m
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
     , HasObjectKey r ObjectKey
     , AWSRequest a
     , MonadLogger m
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
     , HasObjectKey r ObjectKey
     , MonadLogger m
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
     , HasObjectKey r ObjectKey
     , MonadLogger m
     )
  => AWS t
  -> (Status -> m (Maybe L.LogLevel, b))
  -> (t -> m b)
  -> m b
runLoggingAWS action onErr onSucc = do
  conf <- ask
  eResp <- runAWS conf $ trying _Error $ action
  case eResp of
    Left err -> do
      (errMsg, status) <-
        case err of
          TransportError exc -> do
            unless ((conf ^. minLogLevel) == L.LevelDebug) $
              logAWS LevelError "Critical HTTPException"
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
    let speed = (toInteger (stepSum + chunkSize) % 1) / toRational (diffUTCTime curTime prevTime)
    _ <- reporter perc $ round (fromRational @Double speed)
    return (restThresh, 0, curSum, curTime)
  where
    curSum = prevSum + chunkSize
    (perc, curThresh):restThresh = thresh


-- | Helper action that returns a function that can do info level logging in the `MonadIO`.
getInfoLoggerIO :: (MonadIO n, MonadLoggerIO m) => m (Text -> n ())
getInfoLoggerIO = do
  loggerIO <- askLoggerIO
  return $ \ txt -> liftIO $ loggerIO defaultLoc "" LevelInfo (toLogStr txt)


-- | Creates a conduit that will execute supplied action 10 time each for every 10% of the data is
-- being passed through it. Supplied action will receive `Text` with status and speed of processing.
getProgressReporter ::
     MonadIO m => (Text -> m ()) -> Word64 -> ConduitM S.ByteString S.ByteString m ()
getProgressReporter reporterTxt totalSize = do
  let thresh = [(p, (totalSize * p) `div` 100) | p <- [10,20 .. 100]]
      reporter perc speed =
        reporterTxt $
        "Progress: " <> T.pack (show perc) <> "%, speed: " <> formatBytes (fromIntegral speed) <>
        "/s"
      reportProgressAccum chunk acc = do
        acc' <- reportProgress reporter acc (fromIntegral (S.length chunk))
        return (acc', chunk)
  curTime <- liftIO getCurrentTime
  void $ CL.mapAccumM reportProgressAccum (thresh, 0, 0, curTime)


formatBytes :: Integer -> Text
formatBytes val =
  fmt $ fromMaybe (P.last scaled) $ listToMaybe $ P.dropWhile ((>= 10240) . fst) $ scaled
  where
    fmt (sVal10, n) =
      (\(d, m) -> T.pack (show d) <> "." <> T.pack (show m)) (sVal10 `divMod` 10) <> " " <> n
    val10 = 10 * val
    scale (s, r) =
      s +
      if r < 512
        then 0
        else 1
    scaled =
      P.map (\(t, abbr) -> (scale (val10 `divMod` t), abbr)) $
      P.zip
        [2 ^ (x * 10) | x <- [0 :: Int ..]]
        ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]
