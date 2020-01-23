{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
  , mkCacheS3LogFunc
  , module Network.AWS.S3.Cache.Types
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.AWS hiding (LogLevel)
import RIO.Text as T
import Data.Version (Version)
import Data.ByteString.Builder.Extra (flush)
import Network.AWS.S3.Cache.Local
import Network.AWS.S3.Cache.Remote
import Network.AWS.S3.Cache.Stack
import Network.AWS.S3.Cache.Types
import qualified Paths_cache_s3 as Paths
import Prelude as P
import RIO hiding ((^.))
import RIO.Time


showLogLevel :: RIO.LogLevel -> Utf8Builder
showLogLevel level =
  case level of
    LevelDebug   -> "Debug"
    LevelInfo    -> "Info "
    LevelWarn    -> "Warn "
    LevelError   -> "Error"
    LevelOther o -> "Other " <> display o

mkCacheS3LogFunc ::
     Handle
  -> Bool -- ^ Should logger be concise (ommit timestamp and app name)
  -> LogLevel -- ^ Minimum log level
  -> LogFunc
mkCacheS3LogFunc handle' con minLevel =
  mkLogFunc $ \_callstack _logSource logLevel msg ->
    when (logLevel >= minLevel) $ do
      let levelStr = "[" <> showLogLevel logLevel <> "]"
      entryPrefix <-
        if con
          then return $ levelStr <> ": "
          else do
            now <- getCurrentTime
            return $ "[cache-s3]" <> levelStr <> "[" <> formatRFC822 now <> "]: "
      hPutBuilder handle' $ getUtf8Builder (entryPrefix <> msg <> "\n") <> flush
      when (logLevel == LevelError) $ exitWith (ExitFailure 1)


saveCache :: Bool -> Text -> Compression -> [FilePath] -> [FilePath] -> RIO Config ()
saveCache isPublic hAlgTxt comp dirs relativeDirs =
  let hashNoSupport sup =
        logError $
        "Hash algorithm '" <> display hAlgTxt <> "' is not supported, use one of these instead: " <>
        display (T.intercalate ", " sup)
   in withHashAlgorithm_ hAlgTxt hashNoSupport $ \hAlg ->
        withSystemTempFile (makeTempFileNamePattern comp) $ \fp hdl ->
          let tmpFile = TempFile fp hdl comp
           in do sizeAndHash <- writeCacheTempFile dirs relativeDirs hAlg tmpFile
                 uploadCache isPublic tmpFile sizeAndHash



withConfig :: CommonArgs -> (Config -> RIO App a) -> RIO App a
withConfig CommonArgs {..} innerAction = do
  envInit <- liftIO $ newEnv Discover
  let env = maybe envInit (\reg -> envInit & envRegion .~ reg) commonRegion
  mGitBranch <- maybe (getBranchName commonGitDir) (return . Just) commonGitBranch
  let objKey = mkObjectKey commonPrefix mGitBranch commonSuffix
  app <- ask
  innerAction $
    Config
      commonBucket
      objKey
      env
      commonVerbosity
      commonConcise
      Nothing
      commonMaxBytes
      commonNumRetries
      app


runCacheS3 :: CommonArgs -> Action -> RIO App ()
runCacheS3 ca@CommonArgs {..} action = do
  let caAddSuffix suf = ca {commonSuffix = ((<> ".") <$> commonSuffix) <> Just suf}
      caStackSuffix res = caAddSuffix (res <> ".stack")
      caStackWorkSuffix res = caAddSuffix (res <> ".stack-work")
  case action of
    Save SaveArgs {..} ->
      withConfig ca $ \config ->
        runRIO config $ saveCache savePublic saveHash saveCompression savePaths saveRelativePaths
    SaveStack SaveStackArgs {..} -> do
      stackGlobalPaths <- getStackGlobalPaths saveStackRoot
      resolver <- getStackResolver saveStackProject
      runCacheS3 (caStackSuffix resolver) $
        Save saveStackArgs {savePaths = savePaths saveStackArgs ++ stackGlobalPaths}
    SaveStackWork SaveStackWorkArgs {..} -> do
      let SaveStackArgs {..} = saveStackWorkArgs
          StackProject {..} = saveStackProject
      stackLocalPaths <- getStackWorkPaths saveStackRoot stackYaml saveStackWorkDir
      resolver <- getStackResolver saveStackProject
      runCacheS3 (caStackWorkSuffix resolver) $
        Save saveStackArgs {savePaths = savePaths saveStackArgs ++ stackLocalPaths}
    Restore RestoreArgs {..} ->
      withConfig ca $ \config -> do
        restoreSuccessfull <-
          runRIO (config & maxAge .~ restoreMaxAge) $ restoreCache restoreOverwrite
        case (restoreSuccessfull, restoreBaseBranch) of
          (False, Just _)
            | restoreBaseBranch /= commonGitBranch -> do
              app <- ask
              let baseObjKey = mkObjectKey commonPrefix restoreBaseBranch commonSuffix
                  config' =
                    Config
                      commonBucket
                      baseObjKey
                      (config ^. environment)
                      commonVerbosity
                      commonConcise
                      restoreMaxAge
                      commonMaxBytes
                      commonNumRetries
                      app
              void $ runRIO config' $ restoreCache restoreOverwrite
          _ -> return ()
    RestoreStack RestoreStackArgs {..} -> do
      resolver <- getStackResolver restoreStackProject
      runCacheS3 (caStackSuffix resolver) (Restore restoreStackArgs)
    RestoreStackWork RestoreStackArgs {..} -> do
      resolver <- getStackResolver restoreStackProject
      runCacheS3 (caStackWorkSuffix resolver) (Restore restoreStackArgs)
    Clear -> withConfig ca (`runRIO` deleteCache)
    ClearStack proj -> do
      resolver <- getStackResolver proj
      withConfig (caStackSuffix resolver) (`runRIO` deleteCache)
    ClearStackWork proj -> do
      resolver <- getStackResolver proj
      withConfig (caStackWorkSuffix resolver) (`runRIO` deleteCache)

cacheS3Version :: Version
cacheS3Version = Paths.version
