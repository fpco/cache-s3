{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- Module      : Network.AWS.S3.Cache.Stack
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
module Network.AWS.S3.Cache.Stack where

import           Control.Exception          (throwIO)
import           Data.Aeson
import           Data.Git
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.String
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Data.Yaml
import           System.Environment
import           System.FilePath
import           System.Process

getStackRootArg :: Maybe FilePath -> [FilePath]
getStackRootArg = maybe [] (\stackRoot -> ["--stack-root", stackRoot])

getStackPath :: [String] -> FilePath -> IO FilePath
getStackPath args pName =
  concat . filter (not . null) . lines <$>
  readProcess "stack" (args ++ ["path"] ++ [pName]) ""


getStackGlobalPaths :: Maybe FilePath -- ^ Stack root directory
                    -> IO [FilePath]
getStackGlobalPaths mStackRoot = do
  mapM (getStackPath (getStackRootArg mStackRoot)) ["--stack-root", "--local-bin", "--programs"]


getStackWorkPaths :: Maybe FilePath -- ^ Stack root
                  -> Maybe FilePath -- ^ Path to --stack-yaml
                  -> Maybe FilePath -- ^ Relative path for --work-dir
                  -> IO [FilePath]
getStackWorkPaths mStackRoot mStackYaml mWorkDir = do
  let args = getStackRootArg mStackRoot
      fromStr (String str) = Just $ T.unpack str
      fromStr _            = Nothing
  stackYaml <-
    case mStackYaml of
      Just stackYaml -> return stackYaml
      Nothing        -> getStackPath args "--config-location"
  projectRoot <- getStackPath (args ++ ["--stack-yaml", stackYaml]) "--project-root"
  workDir <-
    case mWorkDir of
      Just workDir -> return workDir
      Nothing      -> fromMaybe ".stack-work" <$> lookupEnv "STACK_WORK"
  eObj <- decodeFileEither stackYaml
  pathPkgs <-
    case eObj of
      Left exc -> throwIO exc
      Right (Object (HM.lookup "packages" -> mPackages)) | isJust mPackages ->
        case mPackages of
          Just (Array v) -> return $ V.toList (V.mapMaybe fromStr v)
          _ -> error $ "Expected 'packages' to be a list in the config: " ++ stackYaml
      _ -> error $ "Couldn't find 'packages' in the config: " ++ stackYaml
  return $ map (\pkg -> projectRoot </> pkg </> workDir) pathPkgs


upgradeStack :: Maybe FilePath -- ^ Stack root
             -> IO ()
upgradeStack mStackRoot =
  callProcess "stack" (getStackRootArg mStackRoot ++ ["upgrade"])


-- | Will do its best to find the git repo and get the current branch name, unless GIT_BRANCH env var is
-- set, in which case its value is returned.
getBranchName :: Maybe (FilePath) -- ^ Path to @.git@ repo. Current path will be traversed upwards
                                  -- in search for one if `Nothing` is supplied.
              -> IO (Maybe T.Text)
getBranchName mGitPath = do
  mBranchName <- lookupEnv "GIT_BRANCH"
  case mBranchName of
    Just branchName -> return $ Just $ T.pack branchName
    Nothing ->
      either (const Nothing) (Just . T.pack . refNameRaw) <$>
      case mGitPath of
        Nothing -> withCurrentRepo headGet
        Just fp -> withRepo (fromString fp) headGet
