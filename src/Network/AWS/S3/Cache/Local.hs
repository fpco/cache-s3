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

import Conduit (PrimMonad)
import Control.Exception.Safe (MonadCatch, MonadThrow, throwString)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger
import Control.Monad.Trans.Resource (MonadResource)
import Crypto.Hash (Digest, HashAlgorithm)
import Crypto.Hash.Conduit
import Data.ByteString as S
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List as C
import Data.Conduit.Tar
import Data.List as L
import Data.Maybe as Maybe
import Data.Monoid ((<>))
import Data.Text as T
import Data.Word
import Network.AWS.S3.Cache.Types
import Prelude as P
import System.Directory
import System.FilePath
import System.IO hiding (openTempFile)


tarFiles ::
     (MonadCatch m, MonadResource m, MonadIO m)
  => (LogLevel -> Text -> IO ())
  -> [FilePath]
  -> [FilePath]
  -> ConduitM a ByteString m ()
tarFiles logger dirs relativeDirs = do
  liftIO $ logger LevelDebug "Preparing files for saving in the cache."
  dirsCanonical <- liftIO $ P.mapM canonicalizePath dirs
  uniqueDirs <- Maybe.catMaybes <$> P.mapM skipMissing (removeSubpaths dirsCanonical ++ relativeDirs)
  if P.null uniqueDirs
    then liftIO $ logger LevelError "No paths to cache has been specified."
    else sourceList uniqueDirs .| filePathConduit .| void tar
  where
    skipMissing fp = do
      exist <- liftIO $ doesPathExist fp
      if exist
        then do
          liftIO $ logger LevelInfo $ "Caching: " <> T.pack fp
          return $ Just fp
        else do
          liftIO $ logger LevelWarn $ "File path is skipped since it is missing: " <> T.pack fp
          return Nothing


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
     (HashAlgorithm h, MonadCatch m, MonadResource m, PrimMonad m, MonadThrow m)
  =>
     (LogLevel -> Text -> IO ())
  -> [FilePath]
  -> [FilePath]
  -> h
  -> TempFile
  -> m (Word64, Digest h)
writeCacheTempFile logger dirs relativeDirs _ tmpFile =
  runConduit $ tarFiles logger dirs relativeDirs .| prepareCache tmpFile


-- | Restores all of the files from the tarball and computes the hash at the same time.
restoreFilesFromCache ::
     (MonadIO m, MonadThrow m, MonadResource m, PrimMonad m, HashAlgorithm h)
  => FileOverwrite
  -> (LogLevel -> Text -> IO ())
  -> Compression -- ^ Compression algorithm the stream is expected to be compressed with.
  -> h -- ^ Hashing algorithm to use for computation of hash value of the extracted tarball.
  -> ConduitM ByteString Void m (Digest h)
restoreFilesFromCache (FileOverwrite level) logger comp _ =
  getDeCompressionConduit comp .|
  getZipConduit (ZipConduit (untarWithFinalizers restoreFile') *> ZipConduit sinkHash)
  where
    restoreFile' fi = do
      case fileType fi of
        FTDirectory -- Make sure nested folders are created:
         -> liftIO $ createDirectoryIfMissing True (decodeFilePath (filePath fi))
        FTNormal -> do
          let fp = getFileInfoPath fi
          fileExist <- liftIO $ doesFileExist fp
          when fileExist $ do
            when (level == LevelError) $
              throwString $ "File with name already exists: " ++ fp
            liftIO $ logger level $ "Restoring an existing file: " <> T.pack fp
        _ -> return ()
      restoreFile fi
