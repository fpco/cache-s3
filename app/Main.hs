{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative as A
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text as T
import Data.Version (Version, showVersion)
import Network.AWS hiding (LogLevel)
import Network.AWS.Auth
import Network.AWS.Data
import Network.AWS.S3.Cache
import Network.AWS.S3.Types
import Options.Applicative
import Prelude as P
import System.Environment
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)


readWithMaybe :: (Text -> Maybe a) -> ReadM (Maybe a)
readWithMaybe f = Just <$> maybeReader (f . T.pack)

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

commonArgsParser :: Version -> Maybe String -> Parser CommonArgs
commonArgsParser version mS3Bucket =
  CommonArgs . BucketName . T.pack <$>
  strOption
    (long "bucket" <> short 'b' <> metavar "S3_BUCKET" <> maybe mempty value mS3Bucket <>
     help
       "Name of the S3 bucket that will be used for caching of local files. \
       \If S3_BUCKET environment variable is not set, this argument is required.") <*>
  option
    (Just <$> readRegion)
    (long "region" <> short 'r' <> value Nothing <> metavar "AWS_REGION" <>
     help
       "Region where S3 bucket is located. \
        \By default 'us-east-1' will be used unless AWS_REGION environment variable \
        \is set or defualt region is specified in ~/.aws/config") <*>
  option
    (Just <$> readText)
    (long "prefix" <> value Nothing <> metavar "PREFIX" <>
     help
       "Arbitrary prefix that will be used for storing objects, usually the project \
        \name that this tool is being used for.") <*>
  option
    (Just <$> str)
    (long "git-dir" <> value Nothing <> metavar "GIT_DIR" <>
     help
       "Path to .git repository. Default is either extracted from GIT_DIR environment \
        \variable or current path is traversed upwards in search for one. This argument \
        \is only used for inferring --git-branch, thus it is ignored whenever a custom \
        \value for above argument is specified.") <*>
  option
    (Just <$> readText)
    (long "git-branch" <> value Nothing <> metavar "GIT_BRANCH" <>
     help
       "Current git branch. By default will use the branch the HEAD of repository is \
        \pointing to, unless GIT_BRANCH environment variables is set. This argument is \
        \used for proper namespacing on S3.") <*>
  option
    (Just <$> readText)
    (long "suffix" <> value Nothing <> metavar "SUFFIX" <>
     help
       "Arbitrary suffix that will be used for storing objects in the S3 bucket. \
        \This argument should be used to store multiple cache objects within the \
        \same CI build.") <*>
  option
    readLogLevel
    (long "verbosity" <> metavar "LEVEL" <> short 'v' <> value LevelInfo <>
     help
       "Verbosity level (debug|info|warn|error). Default level is 'info'. \
       \IMPORTANT: Level 'debug' can leak sensitive request information, thus \
       \should NOT be used in production.") <*>
  switch
    (long "concise" <> short 'c' <>
     help "Shorten the output by removing timestamp and name of the tool.") <*>
  option
    (readWithMaybe parseBytes)
    (long "max-size" <> metavar "SIZE" <> value Nothing <>
     help
       "Maximum size of cache that will be acceptable for uploading/downloading to/from S3. \
          \Examples: 5Gb, 750mb, etc. ") <*
  infoOption
    ("cache-s3-" <> showVersion version)
    (long "version" <> help "Print current verison of the program.")


saveArgsParser :: (Parser FilePath -> Parser [FilePath]) -> Parser SaveArgs
saveArgsParser paths =
  SaveArgs <$>
  paths
    (option
       str
       (long "path" <> metavar "PATH" <> short 'p' <> help "All the paths that should be cached")) <*>
  paths
    (option
       str
       (long "relative-path" <> metavar "PATH" <> short 'l' <>
        help "All the relative paths that should be cached")) <*>
  option
    readText
    (long "hash" <> metavar "ALGORITHM" <> value "sha256" <>
     help "Hashing algorithm to use for cache validation (default is 'sha256')") <*>
  option
    (maybeReader (readCompression . T.pack))
    (long "compression" <> metavar "ALGORITHM" <> value GZip <>
     help
       ("Compression algorithm to use for cache. Default 'gzip'. Supported: " <>
        T.unpack supportedCompression)) <*>
  switch
    (long "public" <>
     help
       "Make cache publicly readable. IMPORTANT: Make sure you know what you are \
       \doing when using this flag as it will lead to cache be readable by \
       \anonymous users, which will in turn also result in charges by AWS.")


restoreArgsParser :: Parser RestoreArgs
restoreArgsParser =
  RestoreArgs <$>
  option
    (Just <$> readText)
    (long "base-branch" <> value Nothing <>
     help
       "Base git branch. This branch will be used as a readonly fallback upon a \
          \cache miss, eg. whenever it is a first build for a new branch, it is possible \
          \to use cache from 'master' branch by setting --base-branch=master") <*>
  option
    (readWithMaybe parseDiffTime)
    (long "max-age" <> value Nothing <>
     help
       "Amount of time cache will be valid for from the moment it was initially uploaded to S3, \
        \i.e. updating cache doesn't reset the time counter. Accepts common variations of \
        \(year|day|hour|min|sec), \
        \, eg. --max-age='30 days 1 hour' or --max-age='1h 45m'") <*>
  (FileOverwrite <$>
   option
     readLogLevel
     (long "overwrite" <> metavar "OVERWRITE" <> value LevelDebug <>
      help
        "Which log level to emmit when overwriting an existing file (debug|info|warn|error). \
        \If set to 'error', restoring will be terminated whenever an existing file is detected. \
        \Default is 'debug'."))


stackRootArg :: Parser (Maybe FilePath)
stackRootArg =
  option
    (Just <$> str)
    (long "stack-root" <> value Nothing <> metavar "STACK_ROOT" <>
     help "Global stack directory. Default is taken from stack, i.e a value of \
          \STACK_ROOT environment variable or a system dependent path: eg. \
          \~/.stack/ on Linux, C:\\sr on Windows")


stackProjectParser :: Parser StackProject
stackProjectParser =
  StackProject <$>
  option
    (Just <$> str)
    (long "stack-yaml" <> value Nothing <> metavar "STACK_YAML" <>
     help
       "Path to stack configuration file. Default is taken from stack: i.e. \
       \STACK_YAML environment variable or ./stack.yaml") <*>
  option
    (Just <$> readText)
    (long "resolver" <> value Nothing <> metavar "RESOLVER" <>
     help
       "Use a separate namespace for each stack resolver. Default value is \
       \inferred from stack.yaml")


saveStackArgsParser :: Parser SaveStackArgs
saveStackArgsParser =
  SaveStackArgs <$> saveArgsParser many <*> stackRootArg <*> stackProjectParser


saveStackWorkArgsParser :: Parser SaveStackWorkArgs
saveStackWorkArgsParser =
  subparser $
  metavar "work" <>
  command
    "work"
    (info
       (SaveStackWorkArgs <$> saveStackArgsParser <*>
        option
          (Just <$> str)
          (long "work-dir" <> value Nothing <> metavar "STACK_WORK" <>
           help
             "Relative stack work directory. Default is taken from stack, i.e. \
           \STACK_WORK environment variable or ./.stack-work/") <*
        helpOption)
       (progDesc
          "Command for caching content of .stack-work directory in the S3 bucket. \
       \For projects with many packages, all of the .stack-work directories will \
       \be saved." <>
        fullDesc))


actionParser :: Parser Action
actionParser =
  subparser
    (metavar "save" <>
     command
       "save"
       (info
          (Save <$> saveArgsParser many <* helpOption <|> saveStackCommandParser)
          (progDesc "Command for caching the data in the S3 bucket." <> fullDesc))) <|>
  subparser
    (metavar "restore" <>
     command
       "restore"
       (info
          (restoreStackCommandParser <|> Restore <$> restoreArgsParser <* helpOption)
          (progDesc "Command for restoring cache from S3 bucket." <> fullDesc))) <|>
  clearParser
    (pure Clear)
    "clear"
    "Clears out cache from S3 bucket. This command uses the same arguments as \
     \`cache-s3 save` to uniquely identify the object on S3, therefore same arguments and \
     \subcommands must be suppied in order to clear out the cache created with \
     \`save` command."
    (clearParser
       (ClearStack <$> stackProjectParser)
       "stack"
       "Clear stack cache"
       (clearParser
          (ClearStackWork <$> stackProjectParser)
          "work"
          "Clear stack project work cache"
          A.empty))
  where
    clearParser argsParser com desc altPreParse =
      subparser $
      metavar com <>
      command com (info (altPreParse <|> argsParser <* helpOption) (progDesc desc <> fullDesc))
    saveStackParser = SaveStack <$> saveStackArgsParser <* helpOption
    saveStackCommandParser =
      subparser $
      metavar "stack" <>
      command
        "stack"
        (info
           (SaveStackWork <$> saveStackWorkArgsParser <|> saveStackParser)
           (progDesc
              "Command for caching global stack data in the S3 bucket. This will \
            \include stack root directory and a couple of others that are used \
            \by stack for storing executables. In order to save local .stack-work \
            \directory(ies), use `cache-s3 save stack work` instead." <>
            fullDesc))
    restoreStackArgsParser =
      RestoreStackArgs <$> restoreArgsParser <*> stackRootArg <*> stackProjectParser
    restoreStackCommandParser =
      subparser $
      metavar "stack" <>
      command
        "stack"
        (info
           (restoreStackWorkParser <|> RestoreStack <$> restoreStackArgsParser <* helpOption)
           (progDesc "Command for restoring stack data from the S3 bucket." <> fullDesc))
    restoreStackWorkParser =
      subparser $
      metavar "work" <>
      command
        "work"
        (info
           (RestoreStackWork <$> restoreStackArgsParser <* helpOption)
           (progDesc "Command for restoring .stack-work directory(ies) from the S3 bucket." <>
            fullDesc))


main :: IO ()
main = do
  s3Bucket <- lookupEnv "S3_BUCKET"
  cFile <- credFile
  Args commonArgs acts <-
    execParser $
    info
      (Args <$> commonArgsParser cacheS3Version s3Bucket <*> actionParser <*
       abortOption ShowHelpText (long "help" <> short 'h' <> help "Display this message."))
      (header "cache-s3 - Use an AWS S3 bucket for caching your build environment" <>
       progDesc
         ("Save local directories to S3 bucket and restore them later to their original \
          \locations. AWS credentials will be extracted form the environment in a similar \
          \way that aws-cli does it: you can either place them in " <>
          cFile <>
          " or set them as environment variables " <>
          T.unpack envAccessKey <>
          " and " <>
          T.unpack envSecretKey) <>
       fullDesc)
  hSetBuffering stdout LineBuffering
  runCacheS3 commonArgs acts
