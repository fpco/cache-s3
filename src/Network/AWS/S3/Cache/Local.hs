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

import           Control.Lens
import           Control.Monad                (void)
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Crypto.Hash                  (Digest, HashAlgorithm)
import           Crypto.Hash.Conduit
import           Data.ByteString              as S
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List
import           Data.Conduit.Tar
import           Data.Conduit.Zlib
import           Data.List                    as L --(nub, sort)
import           Data.Monoid                  ((<>))
import           Data.Text                    as T
import           Data.Void
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
  let uniqueDirs = removePrefixSorted $ L.sort dirsCanonical
  if P.null uniqueDirs
    then logErrorN "No paths to cache has been specified."
    else sourceList uniqueDirs .| iterM logPath .| filePathConduit .| void tar
  where
    logPath fp = do
      exist <- liftIO $ doesPathExist fp
      if exist
        then logInfoN $ "Caching: " <> T.pack fp
        else logErrorN $ "File path does not exist: " <> T.pack fp

-- | Avoid saving duplicate files by removing any subpaths. Paths must be canonicalized and sorted.
removePrefixSorted :: Eq a => [[a]] -> [[a]]
removePrefixSorted [] = []
removePrefixSorted (x:xs) = x : L.filter (not . (x `L.isPrefixOf`)) xs


getCompressionConduit :: MonadResource m =>
                         Compression -> Conduit ByteString m ByteString
getCompressionConduit GZip = gzip


getDeCompressionConduit :: MonadResource m =>
                         Compression -> Conduit ByteString m ByteString
getDeCompressionConduit GZip = ungzip


prepareCache ::
     (HashAlgorithm h, MonadResource m)
  => Compression -> ConduitM ByteString Void m (Handle, Digest h, Compression)
prepareCache compression = do
  (_, _, tmpHandle) <- openTempFile Nothing "cache-s3.tar.gz"
  hash <- getZipSink
    (ZipSink (getCompressionConduit compression .| sinkHandle tmpHandle) *> ZipSink sinkHash)
  liftIO $ do
    hFlush tmpHandle
    hSeek tmpHandle AbsoluteSeek 0
  return (tmpHandle, hash, compression)


-- | Create a compressed tarball in the temporary directory. Compute the hash value of the tarball
-- prior to the compression, in order to avoid any possible nondeterminism with future compression
-- algorithms. Returns the computed hash and the file handle where tarball can be read from.
getCacheHandle ::
     ( HashAlgorithm h
     , MonadCatch m
     , MonadResource m
     , MonadLogger m
     )
  => [FilePath]
  -> h
  -> Compression
  -> m (Handle, Digest h, Compression)
getCacheHandle dirs _ comp = runConduit $ tarFiles dirs .| prepareCache comp


-- | Restores all of the files from the tarball and computes the hash at the same time.
restoreFilesFromCache ::
     (HashAlgorithm h, MonadResource m)
  => Compression
  -> h -> ConduitM ByteString Void m (Digest h)
restoreFilesFromCache comp _ =
  getDeCompressionConduit comp .| getZipSink (ZipSink (untarFinally restoreFile) *> ZipSink sinkHash)
