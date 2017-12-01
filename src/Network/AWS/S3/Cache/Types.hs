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
import           Control.Monad
import           Control.Monad.Logger as L
import           Crypto.Hash
import           Data.ByteString      as S
import           Data.Monoid          ((<>))
import           Data.Text            as T
import           Data.Typeable
import           Network.AWS.Env
import           Network.AWS.S3.Types
import           Network.AWS.Types

data Compression
  = GZip
  -- | LZ4
  deriving (Show, Eq)

data Hashing

data Config =
  Config
  { _bucketName    :: !BucketName
  , _objectKey     :: !ObjectKey
  , _confEnv       :: !Env
  }

makeLensesWith classUnderscoreNoPrefixFields ''Config


instance HasEnv Config where
  environment = confEnv



mkObjectKey :: Maybe Text -- ^ Prefix (eg. project name)
            -> Text -- ^ Git branch name
            -> Maybe Text -- ^ Suffix
            -> ObjectKey
mkObjectKey mPrefix branchName mSuffix =
  ObjectKey $ maybe "" (<> "/") mPrefix <> branchName <> maybe "" ("." <>) mSuffix <> ".cache-s3"


hashAlgorithmMetaKey :: Text
hashAlgorithmMetaKey = "hash"

getHashMetaKey :: Typeable h => proxy h -> Text
getHashMetaKey hashProxy = T.toLower (T.pack (showsTypeRep (typeRep hashProxy) ""))


compressionMetaKey :: Text
compressionMetaKey = "compression"

getCompressionName :: Compression -> Text
getCompressionName GZip = "gzip"

readCompression :: Text -> Maybe Compression
readCompression compTxt =
  case T.toLower compTxt of
    "gzip" -> Just GZip
    _      -> Nothing

data CommonArgs = CommonArgs
  { commonRegion    :: !(Maybe Region)
  , commonBucket    :: !BucketName
  , commonPrefix    :: !(Maybe Text)
  , commonBranch    :: !Text
  , commonSuffix    :: !(Maybe Text)
  , commonVerbosity :: !L.LogLevel
  }


data SaveArgs = SaveArgs
  { savePaths       :: ![FilePath]
  , saveHash        :: !Text
  , saveCompression :: !Compression
  } deriving (Show)

data RestoreArgs = RestoreArgs
  { restoreBaseBranch :: !(Maybe Text)
  } deriving (Show)



-- All Actions need:
-- * --aws-key (def from env)
-- * --aws-secret (def from env)
-- * --region | -r (def from env)
-- * --bucket-name | -b (required)
-- * --prefix | -p (optional, def no prefix)
-- * --branch (def current branch)
-- * --verbosity | -v (debug, info, warn, error)
-- * --help | -h
-- * --version
data Action
  = Save SaveArgs
  -- * --paths [path] (or many of --path | -p)
  -- * --hash | -p (def: SHA256)
  -- * --compression | -c (def: gzip)
  | SaveStack
  -- | Same as Save plus:
  -- * (local|global)
  -- * --work-dir (def STACK_WORK -> .stack-work)
  -- * --stack-root (def STACK_ROOT -> call `stack path`)
  -- * --stack-yaml (def STACK_YAML -> call `stack path --config-location` -> stack.yaml)
  | Restore RestoreArgs
  -- * --base-branch (def master)
  | RestoreStack
  -- | Same as restore
  -- --local-only
  -- * [global|local]
  --   * --upgrade (try to upgrade stack on cache hit)
  --   * --install (try to install stack on cache miss)
  | Purge
  deriving (Show)

-- STACK:
-- stack-root: /home/lehins/.stack
-- project-root: /home/lehins/fpco/iohk/cache-s3
-- config-location: /home/lehins/fpco/iohk/cache-s3/stack.yaml


-- | Same as `withHashAlgorithm`, but accepts an extra function to be invoked when unsupported hash
-- algorithm name is supplied that also returns a default value.
withHashAlgorithm_ ::
     forall m a. Monad m
  => Text -- ^ Name of the hash algorithm (case insensitive)
  -> ([Text] -> m a) -- ^ On no hash algorithm n ame support
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

-- bar = 
--   foo (HM.lookup hashAlgorithmMetaKey (resp ^. gorsMetadata)) onNothing $ \ halg ->
--     foo (HM.lookup hashAlgName (resp ^. gorsMetadata)) onNothing' $ \ htxt ->
--        withHashAlgorithm_ hashAlgName noHashAlgSupport $ \hashAlg -> do
--           mHash <- decodeHash hashTxt
--           foo mHash onNothing'' $ \ hash ->
--             foo (HM.lookup compressionMetaKey (resp ^. gorsMetadata)) onNothing''' $ \ compKey ->
--               foo (readCompression compKey) $ \ comp ->
--                 perform action



