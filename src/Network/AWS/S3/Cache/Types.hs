{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
-- |
-- Module      : Network.AWS.S3.Cache.Types
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
module Network.AWS.S3.Cache.Types where

import           Control.Lens
import           Control.Monad.Logger         as L
import           Control.Monad.Trans.Resource (MonadResource)
import           Crypto.Hash
import           Data.ByteString              (ByteString)
import           Data.Conduit                 (Conduit)
import           Data.Conduit.Zlib
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    as T
import           Data.Typeable
import           Network.AWS.Env
import           Network.AWS.S3.Types

#if !WINDOWS
import qualified Data.Conduit.LZ4             as LZ4
#endif

data Config = Config
  { _bucketName  :: !BucketName
  , _objectKey   :: !ObjectKey
  , _confEnv     :: !Env
  , _minLogLevel :: !L.LogLevel
  , _isConcise   :: !Bool
  }

makeLensesWith classUnderscoreNoPrefixFields ''Config


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
  { commonBucket    :: !BucketName
  , commonRegion    :: !(Maybe Region)
  , commonPrefix    :: !(Maybe Text)
  , commonGitDir    :: !(Maybe FilePath)
  , commonGitBranch :: !(Maybe Text)
  , commonSuffix    :: !(Maybe Text)
  , commonVerbosity :: !L.LogLevel
  , commonConcise   :: !Bool
  } deriving (Show)


data SaveArgs = SaveArgs
  { savePaths       :: ![FilePath]
  , saveHash        :: !Text
  , saveCompression :: !Compression
  , savePublic      :: !Bool
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



---------------
--- Hashing ---
---------------


hashAlgorithmMetaKey :: Text
hashAlgorithmMetaKey = "hash"

getHashMetaKey :: Typeable h => proxy h -> Text
getHashMetaKey hashProxy = T.toLower (T.pack (showsTypeRep (typeRep hashProxy) ""))

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


getCompressionConduit :: MonadResource m =>
                         Compression -> Conduit ByteString m ByteString
getCompressionConduit GZip = gzip
#if !WINDOWS
getCompressionConduit LZ4  = LZ4.compress Nothing
#endif


getDeCompressionConduit :: MonadResource m =>
                           Compression -> Conduit ByteString m ByteString
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


