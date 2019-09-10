{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Network.AWS.S3.Cache.Types
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
module Network.AWS.S3.Cache.Types where

import Control.Applicative
import Control.Lens hiding ((<.>))
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Logger as L
import Conduit (PrimMonad, ConduitT)
import Control.Monad.Trans.Resource (MonadResource)
import Crypto.Hash
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as S8
import Data.Char as C (toLower)
import Data.Conduit.Zlib
import Data.Functor (($>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.Encoding as T
import Data.Time
import Data.Typeable
import Network.AWS.Env
import Network.AWS.S3.Types
import Prelude as P
import System.FilePath
import System.IO (Handle)

#if !WINDOWS
import qualified Data.Conduit.LZ4 as LZ4
#endif

data Config = Config
  { _bucketName  :: !BucketName
  , _objectKey   :: !ObjectKey
  , _confEnv     :: !Env
  , _minLogLevel :: !L.LogLevel
  , _isConcise   :: !Bool
  , _maxAge      :: !(Maybe NominalDiffTime)
  , _maxSize     :: !(Maybe Integer)
  , _numRetries  :: !Int
  }

makeLensesWith classUnderscoreNoPrefixFields ''Config

newtype FileOverwrite =
  FileOverwrite L.LogLevel
  deriving (Eq, Show)

instance HasEnv Config where
  environment = confEnv



mkObjectKey :: Maybe Text -- ^ Prefix (eg. project name)
            -> Maybe Text -- ^ Git branch name
            -> Maybe Text -- ^ Suffix
            -> ObjectKey
mkObjectKey mPrefix mBranchName mSuffix =
  ObjectKey $
  "cache-s3/" <> maybe "" (<> "/") mPrefix <> fromMaybe "" mBranchName <> maybe "" ("." <>) mSuffix <>
  ".cache"


data CommonArgs = CommonArgs
  { commonBucket     :: !BucketName
  , commonRegion     :: !(Maybe Region)
  , commonPrefix     :: !(Maybe Text)
  , commonGitDir     :: !(Maybe FilePath)
  , commonGitBranch  :: !(Maybe Text)
  , commonSuffix     :: !(Maybe Text)
  , commonVerbosity  :: !L.LogLevel
  , commonConcise    :: !Bool
  , commonMaxBytes   :: !(Maybe Integer)
  , commonNumRetries :: !Int
  } deriving (Show)


data SaveArgs = SaveArgs
  { savePaths         :: ![FilePath]
  , saveRelativePaths :: ![FilePath]
  , saveHash          :: !Text
  , saveCompression   :: !Compression
  , savePublic        :: !Bool
  } deriving (Show)

data SaveStackArgs = SaveStackArgs
  { saveStackArgs    :: !SaveArgs
  , saveStackRoot    :: !(Maybe FilePath)
  , saveStackProject :: !StackProject
  } deriving (Show)

data SaveStackWorkArgs = SaveStackWorkArgs
  { saveStackWorkArgs :: !SaveStackArgs
  , saveStackWorkDir  :: !(Maybe FilePath)
  } deriving (Show)


data RestoreArgs = RestoreArgs
  { restoreBaseBranch :: !(Maybe Text)
  , restoreMaxAge     :: !(Maybe NominalDiffTime)
  , restoreOverwrite  :: !FileOverwrite
  } deriving (Show)


data StackProject = StackProject
  { stackYaml     :: !(Maybe FilePath)
  , stackResolver :: !(Maybe Text)
  } deriving (Show)



data RestoreStackArgs = RestoreStackArgs
  { restoreStackArgs    :: !RestoreArgs
  , restoreStackRoot    :: !(Maybe FilePath)
  , restoreStackProject :: !StackProject
  } deriving (Show)



data Action
  = Save !SaveArgs
  | SaveStack !SaveStackArgs
  | SaveStackWork !SaveStackWorkArgs
  | Restore !RestoreArgs
  | RestoreStack !RestoreStackArgs
  | RestoreStackWork !RestoreStackArgs
  | Clear
  | ClearStack !StackProject
  | ClearStackWork !StackProject
  deriving (Show)


data TempFile = TempFile
  { tempFilePath        :: !FilePath
  , tempFileHandle      :: !Handle
  , tempFileCompression :: !Compression
  }

makeTempFileNamePattern :: Compression -> FilePath
makeTempFileNamePattern compression = "cache-s3.tar" <.> T.unpack (getCompressionName compression)

----------------
--- Time -------
----------------

data Interval
  = Years Integer
  | Days Integer
  | Hours Integer
  | Minutes Integer
  | Seconds Integer
  deriving (Show)

formatDiffTime :: NominalDiffTime -> Text
formatDiffTime nd = go "" (Seconds <$> divMod (round nd) 60)
  where
    go acc =
      \case
        (_, Years y)
          | y > 0 -> showTime y "year" ", " acc
        (n, Days d)
          | d > 0 || n > 0 -> go (showTime d "day" ", " acc) (0, Years n)
        (n, Hours h)
          | h > 0 || n > 0 -> go (showTime h "hour" ", " acc) (Days <$> divMod n 365)
        (n, Minutes m)
          | m > 0 || n > 0 -> go (showTime m "minute" ", " acc) (Hours <$> divMod n 24)
        (n, Seconds s) -> go (showTime s "second" "" acc) (Minutes <$> divMod n 60)
        _ -> acc
    showTime 0 _    _   acc = acc
    showTime t tTxt sep acc =
      T.pack (show t) <> " " <> tTxt <>
      (if t == 1
         then ""
         else "s") <>
      (if T.null acc
         then acc
         else sep <> acc)

parseDiffTime :: Text -> Maybe NominalDiffTime
parseDiffTime =
  either (const Nothing) (Just . fromInteger . sum . P.map toSec) .
  parseOnly (intervalParser <* skipSpace <* endOfInput) . T.encodeUtf8 . T.toLower
  where
    toSec =
      \case
        Years y -> y * 365 * 24 * 3600
        Days d -> d * 24 * 3600
        Hours h -> h * 3600
        Minutes m -> m * 60
        Seconds s -> s
    maybePlural = (char 's' $> ()) <|> pure ()
    intervalParser =
      many1 $
      skipSpace *>
      choice
        [ Years <$> decimal <* skipSpace <* ("year" <* maybePlural <|> "y")
        , Days <$> decimal <* skipSpace <* ("day" <* maybePlural <|> "d")
        , Hours <$> decimal <* skipSpace <* ("hour" <* maybePlural <|> "h")
        , Minutes <$> decimal <* skipSpace <* ("minute" <* maybePlural <|> "min" <|> "m")
        , Seconds <$> decimal <* skipSpace <* ("second" <* maybePlural <|> "sec" <|> "s")
        ]

formatRFC822 :: UTCTime -> Text
formatRFC822 = T.pack . formatTime defaultTimeLocale rfc822DateFormat

parseISO8601 :: Text -> Maybe UTCTime
parseISO8601 = parseTimeM False defaultTimeLocale iso8601 . T.unpack

formatISO8601 :: UTCTime -> Text
formatISO8601 = T.pack . formatTime defaultTimeLocale iso8601

iso8601 :: String
iso8601 = iso8601DateFormat (Just "%H:%M:%S%Q%z")


parseBytes :: Text -> Maybe Integer
parseBytes =
  either (const Nothing) Just .
  parseOnly ((*) <$> decimal <*> multiplier) . T.encodeUtf8 . T.toLower
  where
    (mults, abbrs) = P.unzip bytesMult
    abbrsParser =
      P.zipWith
        (<|>)
        (P.map (string . S8.pack . P.map C.toLower) abbrs)
        ["", "kb", "mb", "gb", "tb", "pb", "eb", "zb", "yb"]
    multiplier = skipSpace *> choice [p $> f | (p, f) <- P.reverse $ P.zip abbrsParser mults]


formatBytes :: Integer -> Text
formatBytes val =
  fmt $ fromMaybe (P.last scaled) $ listToMaybe $ P.dropWhile ((>= 10240) . fst) scaled
  where
    fmt (sVal10, n) =
      (\(d, m) -> T.pack (show d) <> "." <> T.pack (show m)) (sVal10 `divMod` 10) <> " " <> n
    val10 = 10 * val
    scale (s, r) =
      s +
      if r < 512
        then 0
        else 1
    scaled = [(scale (val10 `divMod` t), T.pack abbr) | (t, abbr) <- bytesMult]

bytesMult :: [(Integer, String)]
bytesMult =
  P.zip
    [2 ^ (x * 10) | x <- [0 :: Int ..]]
    ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]


----------------
--- Matadata ---
----------------

metaCreateTimeKey :: Text
metaCreateTimeKey = "created"

metaHashAlgorithmKey :: Text
metaHashAlgorithmKey = "hash"

getHashMetaKey :: Typeable h => proxy h -> Text
getHashMetaKey hashProxy = T.toLower (T.pack (showsTypeRep (typeRep hashProxy) ""))

---------------
--- Hashing ---
---------------

-- | Same as `withHashAlgorithm`, but accepts an extra function to be invoked when unsupported hash
-- algorithm name is supplied that also returns a default value.
withHashAlgorithm_ ::
     forall m a. Monad m
  => Text -- ^ Name of the hash algorithm (case insensitive)
  -> ([Text] -> m a) -- ^ On no hash algorithm name support
  -> (forall h. (Show h, Typeable h, HashAlgorithm h) => h -> m a)
  -> m a
withHashAlgorithm_ hTxt onErr action = do
  eRes <- withHashAlgorithm hTxt action
  either onErr return eRes

-- | Execute an action with Hash algorithm as an argument, while only supplying its textual name. In
-- case when supplied name is not supported, a list of all valid names will be returned.
withHashAlgorithm ::
     forall m a. Monad m
  => Text -- ^ Name of the hash algorithm (case insensitive)
  -> (forall h. (Show h, Typeable h, HashAlgorithm h) => h -> m a)
  -- ^ Action to invoke with an argument, which is a type that is an isnatnce of `HashAlgorithm`.
  -> m (Either [Text] a)
withHashAlgorithm hTxt action = do
  let hTxtLower = T.toLower hTxt
      tryH :: (Show h, Typeable h, HashAlgorithm h, Monad m) => h -> m (Either [Text] a)
      tryH hAlg =
        let key = getHashMetaKey (Just hAlg)
        in if hTxtLower == key
             then Right <$> action hAlg
             else return $ Left [key]
  tryH SHA256 ?>>
    tryH SHA512 ?>>
    tryH Tiger ?>>
    tryH Skein512_512 ?>>
    tryH Skein512_384 ?>>
    tryH Skein512_256 ?>>
    tryH Skein512_224 ?>>
    tryH Skein256_256 ?>>
    tryH Skein256_224 ?>>
    tryH SHA512t_256 ?>>
    tryH SHA512t_224 ?>>
    tryH SHA384 ?>>
    tryH SHA3_512 ?>>
    tryH SHA3_384 ?>>
    tryH SHA3_256 ?>>
    tryH SHA3_224 ?>>
    tryH SHA224 ?>>
    tryH SHA1 ?>>
    tryH RIPEMD160 ?>>
    tryH MD5 ?>>
    tryH MD4 ?>>
    tryH MD2 ?>>
    tryH Keccak_512 ?>>
    tryH Keccak_384 ?>>
    tryH Keccak_256 ?>>
    tryH Keccak_224 ?>>
    tryH Blake2sp_256 ?>>
    tryH Blake2sp_224 ?>>
    tryH Blake2s_256 ?>>
    tryH Blake2s_224 ?>>
    tryH Blake2s_160 ?>>
    tryH Blake2bp_512 ?>>
    tryH Blake2b_512 ?>>
    tryH Blake2b_384 ?>>
    tryH Blake2b_256 ?>>
    tryH Blake2b_224 ?>>
    tryH Blake2b_160


(?>>) :: (Monoid e, Monad m) => m (Either e a) -> m (Either e a) -> m (Either e a)
(?>>) a1 a2 = do
  eRes1 <- a1
  case eRes1 of
    Left e1 -> do
      eRes2 <- a2
      case eRes2 of
        Left e2   -> return $ Left (e1 <> e2)
        Right res -> return $ Right res
    Right res -> return $ Right res


-------------------
--- Compression ---
-------------------

data Compression
  = GZip
#if !WINDOWS
  | LZ4
#endif
  deriving (Show, Eq, Enum)


getCompressionConduit :: (MonadResource m, MonadThrow m, PrimMonad m) =>
                         Compression -> ConduitT ByteString ByteString m ()
getCompressionConduit GZip = gzip
#if !WINDOWS
getCompressionConduit LZ4  = LZ4.compress Nothing
#endif


getDeCompressionConduit :: (MonadResource m, MonadThrow m, PrimMonad m) =>
                           Compression -> ConduitT ByteString ByteString m ()
getDeCompressionConduit GZip = ungzip
#if !WINDOWS
getDeCompressionConduit LZ4  = LZ4.decompress
#endif


compressionMetaKey :: Text
compressionMetaKey = "compression"

getCompressionName :: Compression -> Text
getCompressionName = T.toLower . T.pack . show

supportedCompression :: Text
supportedCompression = T.intercalate ", " $ getCompressionName <$> enumFrom GZip

readCompression :: Text -> Maybe Compression
readCompression compTxt =
  case T.toLower compTxt of
    "gzip" -> Just GZip
#if !WINDOWS
    "lz4"  -> Just LZ4
#endif
    _      -> Nothing


