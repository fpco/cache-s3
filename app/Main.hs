{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative        as A
import           Data.Attoparsec.Text       (parseOnly)
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Text                  as T
import           Network.AWS                hiding (LogLevel)
import           Network.AWS.Auth
import           Network.AWS.Data
import           Network.AWS.S3.Cache
import           Network.AWS.S3.Types
import           Options.Applicative
import           Prelude                    as P
import           System.Environment
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

commonArgsParser :: Maybe String -> Parser CommonArgs
commonArgsParser mS3Bucket =
  CommonArgs <$>
  (BucketName . T.pack <$>
   (strOption
      (long "bucket" <> short 'b' <> metavar "S3_BUCKET" <> maybe mempty value mS3Bucket <>
       help "Name of the S3 bucket that will be used for caching of local files. \
            \If S3_BUCKET environment variable is not set, this argument is required."))) <*>
  (option
     (readerMaybe readRegion)
     (long "region" <> short 'r' <> value Nothing <>
      help
        "Region where S3 bucket is located. \
        \By default 'us-east-1' will be used unless AWS_REGION environment variable \
        \is set or defualt region is specified in ~/.aws/config")) <*>
  (option
     (readerMaybe readText)
     (long "prefix" <> value Nothing <>
      help
        "Arbitrary prefix that will be used for storing objects, usually the project \
        \name that this tool is being used for.")) <*>
  (option
     (readerMaybe str)
     (long "git-dir" <> value Nothing <> metavar "GIT_DIR" <>
      help
        "Path to .git repository. Default is either extracted from GIT_DIR environment \
        \variable or current path is traversed upwards in search for one. This argument \
        \is only used for inferring --git-branch, thus it is ignored whenever a custom \
        \value for above argument is specified.")) <*>
  (option
     (readerMaybe readText)
     (long "git-branch" <> value Nothing <> metavar "GIT_BRANCH" <>
      help
        "Current git branch. By default will use the branch the HEAD of repository is \
        \pointing to. This is argument is used for proper namespacing on S3.")) <*>
  (option
     (readerMaybe readText)
     (long "suffix" <> value Nothing <>
      help
        "Arbitrary suffix that will be used for storing objects in the S3 bucket. \
        \This argument should be used to store multiple cache objects within the \
        \same CI build.")) <*>
  (option
     readLogLevel
     (long "verbosity" <> short 'v' <> value LevelInfo <>
      help ("Verbosity level (debug|info|warn|error). Default level is 'info'.")))


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
    (long "base-branch" <> value Nothing <>
     help "Base git branch. This branch will be used as a readonly fallback upon a \
          \cache miss, eg. whenever it is a first build for a new branch, it is possible \
          \to use cache from 'master' branch by setting --base-branch=master")


stackRootArg :: Parser (Maybe FilePath)
stackRootArg =
  option
    (readerMaybe str)
    (long "stack-root" <> value Nothing <> metavar "STACK_ROOT" <>
     help "Global stack directory. Default is taken from stack, i.e a value of \
          \STACK_ROOT environment variable or a system dependent path: eg. \
          \~/.stack/ on Linux, C:\\sr on Windows")


stackYamlArgParser :: Parser (Maybe FilePath)
stackYamlArgParser =
  (option
     (readerMaybe str)
     (long "stack-yaml" <> value Nothing <> metavar "STACK_YAML" <>
      help
        "Path to stack configuration file. Default is taken from stack: i.e. \
           \STACK_YAML environment variable or ./stack.yaml"))


stackResolverArgParser :: String -> Parser (Maybe Text)
stackResolverArgParser defStr =
  option
    (readerMaybe readText)
    (long "resolver" <> value Nothing <> metavar "RESOLVER" <>
     help ("Use a separate namespace for each stack resolver." <> defStr))


saveStackArgsParser :: Parser SaveStackArgs
saveStackArgsParser =
  SaveStackArgs <$> saveArgsParser many <*> stackResolverArgParser "" <*> stackRootArg


saveStackWorkArgsParser :: Parser SaveStackWorkArgs
saveStackWorkArgsParser =
  subparser $
  command "work" $
  info
    (SaveStackWorkArgs <$> saveStackArgsParser <*> stackYamlArgParser <*>
     (option
        (readerMaybe str)
        (long "work-dir" <> value Nothing <> metavar "STACK_WORK" <>
         help
           "Relative stack work directory. Default is taken from stack, i.e. \
           \STACK_WORK environment variable or ./.stack-work/")) <*
     helpOption)
    (progDesc
       "Command for caching content of .stack-work directory in the S3 bucket. \
       \For projects with many packages, all of the .stack-work directories will \
       \be saved." <>
     fullDesc)


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
     (restoreStackCommandParser <|> Restore <$> restoreArgsParser <* helpOption)
     (progDesc "Command for restoring cache from S3 bucket." <> fullDesc)) <|>
  (clearParser
     (pure Clear)
     "clear"
     "Clears out cache from S3 bucket. This command uses the same arguments as \
     \`cache-s3 save` to uniquely identify the object on S3, therefore same arguments and \
     \subcommands must be suppied in order to clear out the cache created with \
     \`save` command."
     (clearParser
        (ClearStack <$> (ClearStackArgs <$> stackResolverArgParser ""))
        "stack"
        "Clear stack cache"
        (clearParser
           (ClearStackWork <$>
            (ClearStackWorkArgs <$>
             stackResolverArgParser " Default value is inferred from stack.yaml" <*>
             stackYamlArgParser))
           "work"
           "Clear stack project work cache"
           A.empty)))
  where
    clearParser argsParser com desc altPreParse =
      subparser $
      command com $ info (altPreParse <|> argsParser <* helpOption) (progDesc desc <> fullDesc)
    saveStackParser = SaveStack <$> saveStackArgsParser <* helpOption
    saveStackCommandParser =
      subparser $
      command "stack" $
      info
        (SaveStackWork <$> saveStackWorkArgsParser <|> saveStackParser)
        (progDesc
           "Command for caching global stack data in the S3 bucket. This will \
           \include stack root directory and a couple of others that are used \
           \by stack for storing executables. In order to save local .stack-work \
           \directory(ies), use `cache-s3 save stack work` instead." <>
         fullDesc)
    restoreStackArgsParser =
      RestoreStackArgs <$> restoreArgsParser <*> stackResolverArgParser "" <*>
      (switch
         (long "upgrade" <>
          help
            "After restoring stack and its related files, try to upgrade \
            \stack to newest version.")) <*>
      stackRootArg
    restoreStackCommandParser =
      subparser $
      command "stack" $
      info
        (restoreStackWorkParser <|>
         RestoreStack <$> restoreStackArgsParser <* helpOption)
        (progDesc "Command for restoring stack data from the S3 bucket." <> fullDesc)
    restoreStackWorkParser =
      subparser $
      command "work" $
      info
        (RestoreStackWork <$>
         (RestoreStackWorkArgs <$> restoreArgsParser <*>
          stackResolverArgParser " Default value will be taken inferred from stack.yaml" <*>
          stackYamlArgParser <* helpOption))
        (progDesc "Command for restoring .stack-work directory(ies) from the S3 bucket." <> fullDesc)


main :: IO ()
main = do
  s3Bucket <- lookupEnv "S3_BUCKET"
  cFile <- credFile
  _args@(Args commonArgs acts) <-
    execParser $
    info
      (Args <$> commonArgsParser s3Bucket <*> actionParser <*
       abortOption ShowHelpText (long "help" <> short 'h' <> help "Display this message."))
      (header "cache-s3 - Use AWS S3 bucket as cache for your build environment" <>
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
  --print _args
