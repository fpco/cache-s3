{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.S3.Cache
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
module Network.AWS.S3.Cache where

import Control.Applicative
import Control.Lens
import Control.Monad (when)
import Control.Monad.Logger as L
import Control.Monad.Reader
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Maybe
import Crypto.Hash
import Data.Monoid ((<>))
import Data.Text as T
import Data.Time
import Data.Typeable
import Network.AWS.Auth
import Network.AWS.S3.Cache.Local
import Network.AWS.S3.Cache.Remote
import Network.AWS.S3.Cache.Types
import System.Exit

-- TODO:
-- * Create new logger instead of modifying anotherone
-- * Formal log level prettier
-- * Add option for turninng off the timestamp
-- | Filter out min log level events and add a timestamp to all events.
formatLogger ::
     L.LogLevel -- ^ Minimum log level
  -> LoggingT m a
  -> LoggingT m a
formatLogger minLogLevel (LoggingT f) =
  LoggingT $ \logger ->
    f $ \loc src level msg ->
      when (level >= minLogLevel) $ do
        now <- getCurrentTime
        logger loc src level ("[cache-s3] - [" <> toLogStr (formatRFC822 now) <> "]: " <> msg)
        when (level == LevelError) $ exitWith (ExitFailure 1)

-- | Format `UTCTime` as a `String`.
formatRFC822 :: UTCTime -> String
formatRFC822 = formatTime defaultTimeLocale rfc822DateFormat

saveCache :: Text -> Compression -> [FilePath] -> L.LogLevel -> Config -> IO ()
saveCache hAlgTxt comp dirs minLogLevel conf = do
  let hashNoSupport sup =
        logErrorN $
        "Hash algorithm '" <> hAlgTxt <> "' is not supported, use one of these instead: " <>
        T.intercalate ", " sup
  runResourceT $
    runStdoutLoggingT $
    withHashAlgorithm_ hAlgTxt hashNoSupport $ \hAlg ->
      formatLogger minLogLevel $ runReaderT (getCacheHandle dirs hAlg comp >>= uploadCache) conf

restoreCache :: L.LogLevel -> Config -> IO Bool
restoreCache minLogLevel conf =
  runResourceT $
  runStdoutLoggingT $
  formatLogger minLogLevel $
  runReaderT (maybe False (const True) <$> runMaybeT (downloadCache restoreFilesFromCache)) conf

runCacheS3 :: CommonArgs -> Action -> IO ()
runCacheS3 CommonArgs {..} action = do
  envInit <- newEnv Discover
  let env = maybe envInit (\reg -> envInit & envRegion .~ reg) commonRegion
  let objKey = mkObjectKey commonPrefix commonBranch commonSuffix
  let config = Config commonBucket objKey env
  case action of
    Save (SaveArgs {..}) -> do
      saveCache saveHash saveCompression savePaths commonVerbosity config
    Restore (RestoreArgs {..}) -> do
      restoreSuccessfull <- restoreCache commonVerbosity config
      case (restoreSuccessfull, restoreBaseBranch) of
        (False, Just baseBranch) -> do
          let baseObjKey = mkObjectKey commonPrefix baseBranch commonSuffix
          void $ restoreCache commonVerbosity $ Config commonBucket baseObjKey env
        _ -> return ()
    _ -> error "Not yet unsupported"

cArgs :: CommonArgs
cArgs =
  CommonArgs
    (Just NorthVirginia)
    "lehins-ci-cache"
    (Just "tar-conduit")
    "master"
    (Just "local")
    LevelDebug

uploadDirs :: [FilePath] -> IO ()
uploadDirs dirs = do
  runCacheS3 cArgs (Save (SaveArgs dirs "sha256" GZip))

downloadDirs :: IO ()
downloadDirs = do
  runCacheS3 cArgs (Restore (RestoreArgs Nothing))
