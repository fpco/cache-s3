{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Network.AWS.S3.Cache.Stack
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
module Network.AWS.S3.Cache.Stack where

import Data.Aeson
import Data.Git
import qualified RIO.HashMap as HM
import Data.String
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import Data.Text.Encoding.Error (strictDecode)
import qualified RIO.Vector as V
import Data.Yaml
import Network.AWS.S3.Cache.Types
import RIO
import RIO.FilePath
import RIO.Process

getStackRootArg :: Maybe FilePath -> [FilePath]
getStackRootArg = maybe [] (\stackRoot -> ["--stack-root", stackRoot])

getStackPath :: (HasProcessContext env, HasLogFunc env)
            => [String] -> FilePath -> RIO env FilePath
getStackPath args pName = T.unpack . T.concat . filter (not . T.null) . T.lines . T.decodeUtf8With strictDecode . BL.toStrict . snd <$> p
  where
    p = proc "stack" ("--no-terminal" : args ++ ["path"] ++ [pName])
        (readProcess_
#if WINDOWS
        -- Ignore stderr due to: https://github.com/commercialhaskell/stack/issues/5038
        . setStderr closed
#endif
        )


getStackGlobalPaths :: (HasProcessContext env, HasLogFunc env)
                    => Maybe FilePath -- ^ Stack root directory
                    -> RIO env [FilePath]
getStackGlobalPaths mStackRoot =
  mapM (getStackPath (getStackRootArg mStackRoot)) ["--stack-root", "--programs"]


getStackResolver :: (HasProcessContext env, HasLogFunc env)
                => StackProject -> RIO env T.Text
getStackResolver StackProject { stackResolver = Just resolver } = pure resolver
getStackResolver StackProject {stackYaml = mStackYaml} = do
  yaml <- getStackYaml mStackYaml
  eObj <- liftIO $ decodeFileEither yaml
  case eObj of
    Left exc -> throwIO exc
    Right (Object (HM.lookup "resolver" -> mPackages))
      | isJust mPackages ->
        case mPackages of
          Just (String txt) -> return txt
          _ -> error $ "Expected 'resolver' to be a String in the config: " ++ yaml
    _ -> error $ "Couldn't find 'resolver' in the config: " ++ yaml



getStackYaml :: (HasProcessContext env, HasLogFunc env)
             => Maybe FilePath -> RIO env FilePath
getStackYaml =
  \case
    Just yaml -> return yaml
    Nothing -> maybe "stack.yaml" T.unpack <$> lookupEnv "STACK_YAML"


getStackWorkPaths :: (HasProcessContext env, HasLogFunc env)
                  => Maybe FilePath -- ^ Stack root. It is needed in order to prevent stack from
                                    -- starting to install ghc and the rest in case when root folder
                                    -- is custom.
                  -> Maybe FilePath -- ^ Path to --stack-yaml
                  -> Maybe FilePath -- ^ Relative path for --work-dir
                  -> RIO env [FilePath]
getStackWorkPaths mStackRoot mStackYaml mWorkDir = do
  let fromStr (String ".") = Nothing -- Project root will be added separately
      fromStr (String str) = Just $ T.unpack str
      fromStr _            = Nothing
  yaml <- getStackYaml mStackYaml
  projectRoot <-
    getStackPath (getStackRootArg mStackRoot ++ ["--stack-yaml", yaml]) "--project-root"
  workDir <-
    case mWorkDir of
      Just workDir -> return workDir
      Nothing      -> maybe ".stack-work" T.unpack <$> lookupEnv "STACK_WORK"
  eObj <- liftIO $ decodeFileEither yaml
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
getBranchName ::
     (HasProcessContext env, HasLogFunc env)
  => Maybe FilePath -- ^ Path to @.git@ repo. Current path will be traversed upwards in search for
                    -- one if `Nothing` is supplied.
  -> RIO env (Maybe T.Text)
getBranchName mGitPath = do
  mBranchName <- lookupEnv "GIT_BRANCH"
  case mBranchName of
    Just branchName -> return $ Just branchName
    Nothing ->
      either (const Nothing) (Just . T.pack . refNameRaw) <$>
      case mGitPath of
        Nothing -> liftIO $ withCurrentRepo headGet
        Just fp -> liftIO $ withRepo (fromString fp) headGet
