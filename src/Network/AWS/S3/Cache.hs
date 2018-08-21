{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
-- |
-- Module      : Network.AWS.S3.Cache
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
module Network.AWS.S3.Cache
  ( runCacheS3
  , cacheS3Version
  , L.LogLevel(..)
  , module Network.AWS.S3.Cache.Types
  ) where

import           Control.Exception.Safe       (MonadCatch)
import           Control.Lens
import           Control.Monad                (when)
import           Control.Monad.Logger         as L
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.ByteString.Char8        as S8
import           Data.Monoid                  ((<>))
import           Data.Text                    as T
import           Data.Time
import           Data.Version                 (Version)
import           Network.AWS.S3.Cache.Local
import           Network.AWS.S3.Cache.Remote
import           Network.AWS.S3.Cache.Stack
import           Network.AWS.S3.Cache.Types
import qualified Paths_cache_s3               as Paths
import           Prelude                      as P
import           System.Exit
import           System.Log.FastLogger        (fromLogStr)


showLogLevel :: L.LogLevel -> LogStr
showLogLevel level =
  case level of
    LevelDebug   -> "Debug"
    LevelInfo    -> "Info "
    LevelWarn    -> "Warn "
    LevelError   -> "Error"
    LevelOther o -> "Other " <> toLogStr o

customLoggerT ::
     Bool -- ^ Should logger be concise (ommit timestamp and app name)
  -> L.LogLevel -- ^ Minimum log level
  -> LoggingT m a
  -> m a
customLoggerT con minLevel (LoggingT f) = do
  f $ \_ _ level msg ->
    when (level >= minLevel) $ do
      let levelStr = "[" <> showLogLevel level <> "]"
      entryPrefix <-
        if con
          then return $ levelStr <> ": "
          else do
            now <- getCurrentTime
            return $ "[cache-s3]" <> levelStr <> "[" <> toLogStr (formatRFC822 now) <> "]: "
      S8.putStrLn $ fromLogStr (entryPrefix <> msg)
      when (level == LevelError) $ exitWith (ExitFailure 1)



-- | Run the cache action.
run ::
     (MonadBaseControl IO m, MonadIO m)
  => ReaderT Config (LoggingT (ResourceT m)) a
  -> Config
  -> m a
run action conf =
  runResourceT $
  customLoggerT (conf ^. isConcise) (conf ^. minLogLevel) $ runReaderT action conf


saveCache :: Bool -> Text -> Compression -> [FilePath] -> [FilePath] -> Config -> IO ()
saveCache isPublic hAlgTxt comp dirs relativeDirs conf = do
  let hashNoSupport sup =
        logErrorN $
        "Hash algorithm '" <> hAlgTxt <> "' is not supported, use one of these instead: " <>
        T.intercalate ", " sup
  run
    (withHashAlgorithm_ hAlgTxt hashNoSupport $ \hAlg ->
       getCacheHandle dirs relativeDirs hAlg comp >>= (void . runMaybeT . uploadCache isPublic))
    conf


restoreCache :: Config -> IO Bool
restoreCache = run (maybe False (const True) <$> runMaybeT (downloadCache restoreFilesFromCache))

mkConfig :: (MonadIO m, MonadCatch m) =>
            CommonArgs -> m Config
mkConfig CommonArgs {..} = do
  envInit <- newEnv Discover
  let env = maybe envInit (\reg -> envInit & envRegion .~ reg) commonRegion
  mGitBranch <- maybe (liftIO $ getBranchName commonGitDir) (return . Just) commonGitBranch
  let objKey = mkObjectKey commonPrefix mGitBranch commonSuffix
  return $ Config commonBucket objKey env commonVerbosity commonConcise Nothing commonMaxBytes


runCacheS3 :: CommonArgs -> Action -> IO ()
runCacheS3 ca@CommonArgs {..} action = do
  let caAddSuffix suf = ca {commonSuffix = ((<> ".") <$> commonSuffix) <> Just suf}
      caStackSuffix res = caAddSuffix (res <> ".stack")
      caStackWorkSuffix res = caAddSuffix (res <> ".stack-work")
  case action of
    Save (SaveArgs {..}) -> do
      config <- mkConfig ca
      saveCache savePublic saveHash saveCompression savePaths saveRelativePaths config
    SaveStack (SaveStackArgs {..}) -> do
      stackGlobalPaths <- getStackGlobalPaths saveStackRoot
      resolver <- getStackResolver saveStackProject
      runCacheS3 (caStackSuffix resolver) $
        Save saveStackArgs {savePaths = savePaths saveStackArgs ++ stackGlobalPaths}
    SaveStackWork (SaveStackWorkArgs {..}) -> do
      let SaveStackArgs {..} = saveStackWorkArgs
          StackProject {..} = saveStackProject
      stackLocalPaths <- getStackWorkPaths saveStackRoot stackYaml saveStackWorkDir
      resolver <- getStackResolver saveStackProject
      runCacheS3 (caStackWorkSuffix resolver) $
        Save saveStackArgs {savePaths = savePaths saveStackArgs ++ stackLocalPaths}
    Restore (RestoreArgs {..}) -> do
      config <- mkConfig ca
      restoreSuccessfull <- restoreCache (config & maxAge .~ restoreMaxAge)
      case (restoreSuccessfull, restoreBaseBranch) of
        (False, Just _) | restoreBaseBranch /= commonGitBranch -> do
          let baseObjKey = mkObjectKey commonPrefix restoreBaseBranch commonSuffix
          void $
            restoreCache $
            Config
              commonBucket
              baseObjKey
              (config ^. confEnv)
              commonVerbosity
              commonConcise
              restoreMaxAge
              commonMaxBytes
        _ -> return ()
    RestoreStack (RestoreStackArgs {..}) -> do
      resolver <- getStackResolver restoreStackProject
      runCacheS3 (caStackSuffix resolver) (Restore restoreStackArgs)
    RestoreStackWork (RestoreStackArgs {..}) -> do
      resolver <- getStackResolver restoreStackProject
      runCacheS3 (caStackWorkSuffix resolver) (Restore restoreStackArgs)
    Clear -> mkConfig ca >>= run deleteCache
    ClearStack proj -> do
      resolver <- getStackResolver proj
      mkConfig (caStackSuffix resolver) >>= run deleteCache
    ClearStackWork proj -> do
      resolver <- getStackResolver proj
      mkConfig (caStackWorkSuffix resolver) >>= run deleteCache


cacheS3Version :: Version
cacheS3Version = Paths.version
