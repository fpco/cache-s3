{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Network.AWS.S3.Cache.Local
-- Copyright   : (c) FP Complete 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--

module Network.AWS.S3.Cache.Local where

import Control.Monad.Trans.Resource (ResourceT, MonadResource)
import Crypto.Hash (Digest, HashAlgorithm)
import Crypto.Hash.Conduit
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List as C
import Data.Conduit.Tar
import RIO.List as L
import Network.AWS.S3.Cache.Types
import RIO
import RIO.Directory
import RIO.FilePath
import System.IO (SeekMode(AbsoluteSeek))


tarFiles ::
     (HasLogFunc env)
  => [FilePath]
  -> [FilePath]
  -> ConduitM a ByteString (ResourceT (RIO env)) ()
tarFiles dirs relativeDirs = do
  logDebug "Preparing files for saving in the cache."
  dirsCanonical <- RIO.mapM canonicalizePath dirs
  uniqueDirs <-
    RIO.catMaybes <$> RIO.mapM skipMissing (removeSubpaths dirsCanonical ++ relativeDirs)
  if L.null uniqueDirs
    then logError "No paths to cache has been specified."
    else sourceList uniqueDirs .| filePathConduit .| void tar
  where
    skipMissing fp = do
      exist <- doesPathExist fp
      if exist
        then Just fp <$ logInfo ("Caching: " <> fromString fp)
        else Nothing <$ logWarn ("File path is skipped since it is missing: " <> fromString fp)


-- | Will remove any subfolders or files. Imput is expected to be a list of canonicalized file
-- paths.
removeSubpaths :: [FilePath] -> [FilePath]
removeSubpaths dirsCanonical =
  L.map joinPath $ removePrefixSorted $ L.sort $ L.map splitDirectories dirsCanonical
  where
    removePrefixSorted :: [[FilePath]] -> [[FilePath]]
    removePrefixSorted []     = []
    removePrefixSorted (x:xs) = x : L.filter (not . (x `isPathPrefixOf`)) xs
    isPathPrefixOf :: [FilePath] -> [FilePath] -> Bool
    isPathPrefixOf [] _          = True
    isPathPrefixOf _ []          = False
    isPathPrefixOf (x:xs) (y:ys) = equalFilePath x y && isPathPrefixOf xs ys


prepareCache ::
     (HashAlgorithm h, MonadResource m, PrimMonad m, MonadThrow m)
  => TempFile -> ConduitM ByteString Void m (Word64, Digest h)
prepareCache TempFile {tempFileHandle, tempFileCompression} = do
  hash <-
    getZipSink
      (ZipSink (getCompressionConduit tempFileCompression .| sinkHandle tempFileHandle) *>
       ZipSink sinkHash)
  cSize <-
    liftIO $ do
      hFlush tempFileHandle
      cSize <- hTell tempFileHandle
      hSeek tempFileHandle AbsoluteSeek 0
      return cSize
  return (fromInteger cSize, hash)


-- | Create a compressed tarball and write it into a handle. Compute the hash value of the tarball
-- prior to the compression, in order to avoid any possible nondeterminism with future compression
-- algorithms. Returns the computed hash. File handle is set to the beginning of the file so the
-- tarball can be read from.
writeCacheTempFile ::
     (HasLogFunc env, HashAlgorithm h)
  =>
     [FilePath]
  -> [FilePath]
  -> h
  -> TempFile
  -> RIO env (Word64, Digest h)
writeCacheTempFile dirs relativeDirs _ tmpFile =
  runConduitRes $ tarFiles dirs relativeDirs .| prepareCache tmpFile


-- | Restores all of the files from the tarball and computes the hash at the same time.
restoreFilesFromCache ::
     (HasLogFunc env, HashAlgorithm h, MonadReader env m, PrimMonad m, MonadThrow m, MonadIO m)
  => FileOverwrite
  -> Compression -- ^ Compression algorithm the stream is expected to be compressed with.
  -> h -- ^ Hashing algorithm to use for computation of hash value of the extracted tarball.
  -> ConduitM ByteString Void (ResourceT m) (Digest h)
restoreFilesFromCache (FileOverwrite level) comp _ =
  getDeCompressionConduit comp .|
  getZipConduit (ZipConduit (untarWithFinalizers restoreFile') *> ZipConduit sinkHash)
  where
    restoreFile' fi = do
      case fileType fi of
        FTDirectory -- Make sure nested folders are created:
         -> createDirectoryIfMissing True (decodeFilePath (filePath fi))
        FTNormal -> do
          let fp = getFileInfoPath fi
          fileExist <- doesFileExist fp
          when fileExist $ do
            when (level == LevelError) $
              throwString $ "File with name already exists: " ++ fp
            logGeneric "" level $ "Restoring an existing file: " <> fromString fp
        _ -> return ()
      restoreFile fi
