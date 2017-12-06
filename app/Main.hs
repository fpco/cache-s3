{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text       (parseOnly)
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Text                  as T
import           Network.AWS                hiding (LogLevel)
import           Network.AWS.Data
import           Network.AWS.S3.Cache
import           Network.AWS.S3.Types
import           Options.Applicative.Simple
import           Prelude                    as P
import           System.IO                  (BufferMode (LineBuffering),
                                             hSetBuffering, stdout)
import           Text.Read                  (readMaybe)

readerMaybe :: ReadM a -> ReadM (Maybe a)
readerMaybe reader = (Just <$> reader) <|> pure Nothing


maybeAuto :: Read a => ReadM (Maybe a)
maybeAuto =
  maybeReader $ \strVal ->
    case readMaybe strVal of
      Just val -> Just (Just val)
      Nothing  -> Nothing

readLogLevel :: ReadM LogLevel
readLogLevel =
  maybeReader $ \case
    "debug" -> Just LevelDebug
    "info"  -> Just LevelInfo
    "warn"  -> Just LevelWarn
    "error" -> Just LevelError
    _       -> Nothing


data Args = Args CommonArgs Action deriving Show

readText :: ReadM T.Text
readText = T.pack <$> str

readRegion :: ReadM Region
readRegion = do
  strRegion <- str
  either readerError return $ parseOnly parser $ T.pack strRegion


helpOption :: Parser (a -> a)
helpOption = abortOption ShowHelpText (long "help" <> short 'h' <> help "Display this message.")

commonArgsParser :: Parser CommonArgs
commonArgsParser =
  CommonArgs <$>
  (BucketName . T.pack <$>
   strOption (long "bucket" <> short 'b' <>
              help "S3 Bucket name where cache will be uploaded to.")) <*>
  (option
     (readerMaybe readRegion)
     (long "region" <> short 'r' <> value Nothing <>
      help
        ("Region where S3 bucket is located. \
         \By default 'us-east-1' will be used unless AWS_REGION environment variable \
         \is set or defualt region is specified in ~/.aws/config"))) <*>
  (option
     (readerMaybe readText)
     (long "prefix" <> value Nothing <>
      help
        ("Pefix that will be used for storing objects, usually the project name that this \
         \tool is used for."))) <*>
  (option
     (readerMaybe str)
     (long "git-dir" <> value Nothing <> metavar "GIT_DIR" <> help "Path to .git repository")) <*>
  (option
     (readerMaybe readText)
     (long "branch" <> value Nothing <> metavar "GIT_BRANCH" <> help "Git branch")) <*>
  (option
     (readerMaybe readText)
     (long "suffix" <> value Nothing <>
      help
        "Pefix that will be used for storing objects, usually the project name that \
        \the tool is used for.")) <*>
  (option
     readLogLevel
     (long "verbosity" <> short 'v' <> value LevelInfo <>
      help ("Verbosity level (debug|info|warn|error)")))


saveArgsParser :: (Parser FilePath -> Parser [FilePath]) -> Parser SaveArgs
saveArgsParser paths =
  SaveArgs <$>
  paths (option str (long "path" <> short 'p' <> help "All the paths that should be chached")) <*>
  (option
     readText
     (long "hash" <> short 'h' <> value "sha256" <>
      help "Hashing algorithm to use for cache validation")) <*>
  option
    (maybeReader (readCompression . T.pack))
    (long "compression" <> short 'c' <> value GZip <> help "Compression algorithm to use for cache")


restoreArgsParser :: Parser RestoreArgs
restoreArgsParser =
  RestoreArgs <$>
  option
  (readerMaybe readText)
  (long "base-branch" <> help "Base git branch" <> value Nothing)


stackRootArg =
  option
    (readerMaybe str)
    (long "stack-root" <> value Nothing <> metavar "STACK_ROOT" <>
     help "Default STACK_ROOT or ~/.stack")

saveStackArgsParser :: Parser SaveStackArgs
saveStackArgsParser = SaveStackArgs <$> saveArgsParser many <*> stackRootArg


saveStackWorkArgsParser :: Parser SaveStackWorkArgs
saveStackWorkArgsParser =
  subparser $
  command "work" $
  info
    (SaveStackWorkArgs <$> saveStackArgsParser <*>
     (option
        (readerMaybe str)
        (long "stack-yaml" <> value Nothing <> metavar "STACK_YAML" <>
         help "Default STACK_YAML or ./stack.yaml")) <*>
     (option
        (readerMaybe str)
        (long "work-dir" <> value Nothing <> metavar "STACK_WORK" <>
         help "Default STACK_WORK or .stack-work")) <*
     helpOption)
    (progDesc "Command for caching the data in the S3 bucket." <> fullDesc)


actionParser :: Parser Action
actionParser =
  (subparser $
   command "save" $
   info
     (Save <$> saveArgsParser some <* helpOption <|> saveStackCommandParser)
     (progDesc "Command for caching the data in the S3 bucket." <> fullDesc)) <|>
  (subparser $
   command "restore" $
   info
     (Restore <$> restoreArgsParser <* helpOption <|> restoreStackCommandParser)
     (progDesc "Command for restoring cache from S3 bucket." <> fullDesc))
  where
    saveStackParser = SaveStack <$> saveStackArgsParser <* helpOption
    saveStackCommandParser =
      subparser $
      command "stack" $
      info
        (saveStackParser <|> SaveStackWork <$> saveStackWorkArgsParser)
        (progDesc "Command for caching stack data in the S3 bucket." <> fullDesc)
    restoreStackArgsParser =
      RestoreStackArgs <$> restoreArgsParser <*>
      (switch (long "upgrade" <> help "Attempt to upgrade stack to newest version.")) <*>
      stackRootArg
    restoreStackCommandParser =
      subparser $
      command "stack" $
      info
        (RestoreStack <$> restoreStackArgsParser <|> restoreStackWorkParser)
        (progDesc "Command for restoring stack data from the S3 bucket." <> fullDesc)
    restoreStackWorkParser =
      subparser $
      command "work" $
      info
        (RestoreStackWork <$> restoreStackArgsParser)
        (progDesc "Command for restoring stack work directories from the S3 bucket." <> fullDesc)


main :: IO ()
main = do
  _args@(Args commonArgs acts) <-
    execParser $
    info
      (Args <$> commonArgsParser <*> actionParser <*
       abortOption ShowHelpText (long "help" <> short 'h' <> help "Display this message."))
      (header "cache-s3 - Use AWS S3 bucket as cache for your build environment" <>
       progDesc
         "Save local directories to S3 bucket and restore them later to their original locations." <>
       fullDesc)
  hSetBuffering stdout LineBuffering
  runCacheS3 commonArgs acts
  print _args
