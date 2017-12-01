module Main where

import           Data.Maybe
import           Network.AWS.S3.Cache
import           System.Environment

main :: IO ()
main = do
  args@(c:paths) <- getArgs
  case c of
    "save"    | not (null paths) -> uploadDirs paths
    "restore" -> downloadDirs
    _         -> error $ "Invalid args: " ++ show args
