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

import           Control.Exception   (throwIO)
import           Data.Aeson
import           Data.Git
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe, isJust)
import           Data.String
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Data.Yaml
import           Network.AWS.S3.Cache.Types
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
  mapM (getStackPath (getStackRootArg mStackRoot)) ["--stack-root", "--programs"]


getStackResolver :: StackProject -> IO T.Text
getStackResolver (StackProject { stackResolver = Just resolver }) = return resolver
getStackResolver (StackProject { stackYaml = mStackYaml }) = do
  stackYaml <- getStackYaml mStackYaml
  eObj <- decodeFileEither stackYaml
  case eObj of
    Left exc -> throwIO exc
    Right (Object (HM.lookup "resolver" -> mPackages)) | isJust mPackages ->
        case mPackages of
          Just (String txt) -> return txt
          _ -> error $ "Expected 'resolver' to be a String in the config: " ++ stackYaml
    _ -> error $ "Couldn't find 'resolver' in the config: " ++ stackYaml



getStackYaml :: Maybe FilePath -> IO FilePath
getStackYaml mStackYaml =
  case mStackYaml of
    Just stackYaml -> return stackYaml
    Nothing -> fromMaybe "stack.yaml" <$> lookupEnv "STACK_YAML"


getStackWorkPaths :: Maybe FilePath -- ^ Stack root. It is needed in order to prevent stack from
                                    -- starting to install ghc and the rest in case when root folder
                                    -- is custom.
                  -> Maybe FilePath -- ^ Path to --stack-yaml
                  -> Maybe FilePath -- ^ Relative path for --work-dir
                  -> IO [FilePath]
getStackWorkPaths mStackRoot mStackYaml mWorkDir = do
  let fromStr (String ".") = Nothing -- Project root will be added separately
      fromStr (String str) = Just $ T.unpack str
      fromStr _ = Nothing
  stackYaml <- getStackYaml mStackYaml
  projectRoot <-
    getStackPath (getStackRootArg mStackRoot ++ ["--stack-yaml", stackYaml]) "--project-root"
  workDir <-
    case mWorkDir of
      Just workDir -> return workDir
      Nothing -> fromMaybe ".stack-work" <$> lookupEnv "STACK_WORK"
  eObj <- decodeFileEither stackYaml
  pathPkgs <-
    case eObj of
      Left exc -> throwIO exc
      Right (Object obj)
        | Just (Array packages) <- HM.lookup "packages" obj ->
          pure $ V.toList (V.mapMaybe fromStr packages)
      _ -> pure []
  return ((projectRoot </> workDir) : map (\pkg -> projectRoot </> pkg </> workDir) pathPkgs)


-- | Will do its best to find the git repo and get the current branch name, unless GIT_BRANCH env
-- var is set, in which case its value is returned.
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
