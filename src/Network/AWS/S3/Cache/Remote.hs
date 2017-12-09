{-# LANGUAGE CPP                   #-}
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
import           Data.ByteString.Builder              (toLazyByteString)
import           Data.ByteString.Lazy                 as SL
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List                    as CL
import           Data.HashMap.Strict                  as HM
import           Data.Monoid                          ((<>))
import           Data.Ratio                           ((%))
import           Data.Text                            as T
import           Data.Text.Encoding                   as T
import           Data.Text.Encoding.Error             as T
import           Data.Time
import           Data.Typeable
import           Data.Word                            (Word64)
import           Network.AWS
import           Network.AWS.Data.Body
import           Network.AWS.Data.Log                 (build)
import           Network.AWS.S3.Cache.Types
import           Network.AWS.S3.CreateMultipartUpload
import           Network.AWS.S3.DeleteObject
import           Network.AWS.S3.GetObject
import           Network.AWS.S3.StreamingUpload
import           Network.AWS.S3.Types
import           Network.HTTP.Types.Status            (status404)
import           Prelude                              as P
import           System.IO                            (Handle)

#if !WINDOWS
import qualified Formatting                           as F (bytes, fixed,
                                                            sformat, (%))
#endif

hasCacheChanged ::
     ( MonadReader r m
     , MonadResource m
     , MonadLogger m
     , HasBucketName r BucketName
     , HasObjectKey r ObjectKey
     , HasEnv r
     , HashAlgorithm h
     , Typeable h
     )
  => Digest h
  -> m Bool
hasCacheChanged newHash = do
  c <- ask
  let getObjReq = getObject (c ^. bucketName) (c ^. objectKey)
      hashKey = getHashMetaKey newHash
      onErr err = do
        case err of
          ServiceError se
            | se ^. serviceStatus == status404 -> do
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
        "Data change detected, caching " <> formatBytes cSize <> " with " <>
        hashKey <>
        ": " <>
        newHashTxt
      reporter <- getInfoLoggerIO
      runLoggingAWS_ $
        runConduit $
        sourceHandle hdl .| passthroughSink (streamUpload (Just 1024) cmu) (void . pure) .|
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
     , HasObjectKey c ObjectKey
     , HasBucketName c BucketName
     )
  => m ()
deleteCache = do
  c <- ask
  sendAWS_ (deleteObject (c ^. bucketName) (c ^. objectKey)) (void . pure)

-- | Download an object from S3 and handle its content using the supplied sink.
downloadCache ::
     ( MonadResource m
     , MonadLoggerIO m
     , MonadReader c m
     , HasEnv c
     , HasObjectKey c ObjectKey
     , HasBucketName c BucketName
     )
  => (forall h. HashAlgorithm h =>
       Compression -> h -> Sink S.ByteString (ResourceT IO) (Digest h))
  -> MaybeT m ()
downloadCache sink = do
  c <- ask
  let getObjReq = getObject (c ^. bucketName) (c ^. objectKey)
      onErr err = do
        case err of
          ServiceError se
            | se ^. serviceStatus == status404 -> do
              logAWS LevelInfo "No previously stored cache was found."
              MaybeT $ return Nothing
          _ -> return (Just LevelError, ())
  logAWS LevelInfo "Checking for previously stored cache."
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
      logAWS LevelInfo $ "Restoring cache."
      reporter <- getInfoLoggerIO
      let sinkWithProgress =
            maybe
              (sink compAlg hashAlg)
              (\contentSize ->
                 getProgressReporter reporter (fromInteger contentSize) .| sink compAlg hashAlg)
              (resp ^. gorsContentLength)
      hashComputed <-
        liftIO $ runResourceT $ resp ^. gorsBody ^. to _streamBody $$+- sinkWithProgress
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
     (MonadReader r m, MonadResource m, HasEnv r, AWSRequest a, MonadLogger m)
  => a
  -> (Error -> m (Maybe L.LogLevel, b))
  -> (Rs a -> m b)
  -> m b
sendAWS req = runLoggingAWS (send req)


-- | Same as `sendAWS`, but discard the response and simply error out on any received AWS error
-- responses.
sendAWS_ ::
     (MonadReader r m, MonadResource m, HasEnv r, AWSRequest a, MonadLogger m)
  => a
  -> (Rs a -> m ())
  -> m ()
sendAWS_ req = runLoggingAWS (send req) (const $ return (Just LevelError, ()))


-- | Report every problem as `LevelError` and discard the result.
runLoggingAWS_ :: (MonadReader r m, MonadResource m, HasEnv r, MonadLogger m) =>
                  AWS () -> m ()
runLoggingAWS_ action = runLoggingAWS action (const $ pure (Just LevelError, ())) return


-- | General helper for calling AWS and conditionally log the outcome upon a received error.
runLoggingAWS
  :: (MonadReader r m, MonadResource m, HasEnv r, MonadLogger m) =>
     AWS t -> (Error -> m (Maybe L.LogLevel, b)) -> (t -> m b) -> m b
runLoggingAWS action onErr onSucc = do
  env <- ask
  eResp <- runAWS env $ trying _Error $ action
  case eResp of
    Left err -> do
      (mLevel, def) <- onErr err
      case mLevel of
        Just level ->
          logOtherN level $
          T.decodeUtf8With T.lenientDecode $ SL.toStrict $ toLazyByteString $ build err
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


-- | Format bytes into a human readable string
formatBytes :: Word64 -> Text
#if WINDOWS
formatBytes b = T.pack (show b) <> "bytes"
#else
formatBytes = F.sformat (F.bytes @Double (F.fixed 1 F.% " "))
#endif


-- | Creates a conduit that will execute supplied action 10 time each for every 10% of the data is
-- being passed through it. Supplied action will receive `Text` with status and speed of processing.
getProgressReporter ::
     MonadIO m => (Text -> m ()) -> Word64 -> ConduitM S.ByteString S.ByteString m ()
getProgressReporter reporterTxt totalSize = do
  let thresh = [(p, (totalSize * p) `div` 100) | p <- [10,20 .. 100]]
      reporter perc speed =
        reporterTxt $
        "Progress: " <> T.pack (show perc) <> "%, speed: " <> formatBytes speed <> "/s"
      reportProgressAccum chunk acc = do
        acc' <- reportProgress reporter acc (fromIntegral (S.length chunk))
        return (acc', chunk)
  curTime <- liftIO getCurrentTime
  void $ CL.mapAccumM reportProgressAccum (thresh, 0, 0, curTime)


-- _prog :: Int -> (String, Double)
-- _prog val =
--     head $ dropWhile (\(_, sv) -> sv >= 1024) $ map (\(n, t) -> (n, fromIntegral val / t)) $
--     zip ["B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"] [2 ^ (x * 10) | x <- [0 :: Int ..]]

