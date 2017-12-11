{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Network.AWS.S3.Cache.Local
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--

module Network.AWS.S3.Cache.Local where

import           Control.Monad                (void)
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
import           Crypto.Hash                  (Digest, HashAlgorithm)
import           Crypto.Hash.Conduit
import           Data.ByteString              as S
import           Data.ByteString.Char8        as S8
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List            as C
import           Data.Conduit.Tar
import           Data.Foldable                as F (foldrM)
import           Data.List                    as L
import           Data.Monoid                  ((<>))
import           Data.Text                    as T
import           Data.Void
import           Data.Word
import           Network.AWS.S3.Cache.Types
import           Prelude                      as P
import           System.Directory
import           System.IO                    hiding (openTempFile)
import           System.IO.Temp

tarFiles :: (MonadCatch m, MonadResource m, MonadLogger m) =>
            [FilePath] -> ConduitM a ByteString m ()
tarFiles dirs = do
  logDebugN "Preparing files for saving in the cache."
  dirsCanonical <- liftIO $ P.mapM canonicalizePath dirs
  uniqueDirs <- removeMissingM $ removePrefixSorted $ L.sort dirsCanonical
  if P.null uniqueDirs
    then logErrorN "No paths to cache has been specified."
    else sourceList uniqueDirs .| iterM logPath .| filePathConduit .| void tar
  where
    logPath fp = do
      exist <- liftIO $ doesPathExist fp
      if exist
        then logInfoN $ "Caching: " <> T.pack fp
        else logErrorN $ "File path does not exist: " <> T.pack fp
    removeMissingM = F.foldrM skipMissing []
      where
        skipMissing fp acc = do
          exist <- liftIO $ doesPathExist fp
          if exist
            then return (fp : acc)
            else do
              logWarnN $ "File path is skipped since it is missing: " <> T.pack fp
              return acc

-- | Avoid saving duplicate files by removing any subpaths. Paths must be canonicalized and sorted.
removePrefixSorted :: Eq a => [[a]] -> [[a]]
removePrefixSorted []     = []
removePrefixSorted (x:xs) = x : L.filter (not . (x `L.isPrefixOf`)) xs


prepareCache ::
     (HashAlgorithm h, MonadResource m)
  => Compression -> ConduitM ByteString Void m (Handle, Word64, Digest h, Compression)
prepareCache compression = do
  (_, _, tmpHandle) <- openTempFile Nothing "cache-s3.tar"
  hash <- getZipSink
    (ZipSink (getCompressionConduit compression .| sinkHandle tmpHandle) *> ZipSink sinkHash)
  cSize <- liftIO $ do
    hFlush tmpHandle
    cSize <- hTell tmpHandle
    hSeek tmpHandle AbsoluteSeek 0
    return cSize
  return (tmpHandle, fromInteger cSize, hash, compression)


-- | Create a compressed tarball in the temporary directory. Compute the hash value of the tarball
-- prior to the compression, in order to avoid any possible nondeterminism with future compression
-- algorithms. Returns the computed hash and the file handle where tarball can be read from.
getCacheHandle ::
     (HashAlgorithm h, MonadCatch m, MonadResource m, MonadLogger m)
  => [FilePath]
  -> h
  -> Compression
  -> m (Handle, Word64, Digest h, Compression)
getCacheHandle dirs _ comp = runConduit $ tarFiles dirs .| prepareCache comp


-- | Restores all of the files from the tarball and computes the hash at the same time.
restoreFilesFromCache ::
     HashAlgorithm h
  => Compression -- ^ Compression algorithm the stream is expected to be compressed with.
  -> h -- ^ Hashing algorithm to use for computation of hash value of the extracted tarball.
  -> ConduitM ByteString Void (ResourceT IO) (Digest h)
restoreFilesFromCache comp _ =
  getDeCompressionConduit comp .|
  getZipConduit (ZipConduit (untarWithFinalizers restoreFile') *> ZipConduit sinkHash)
  where
    restoreFile' fi = do
      case fileType fi of
        FTDirectory -> liftIO $ createDirectoryIfMissing True (S8.unpack (filePath fi))
        _ -> return ()
      restoreFile fi
