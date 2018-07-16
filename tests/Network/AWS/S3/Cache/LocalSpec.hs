module Network.AWS.S3.Cache.LocalSpec (spec) where

import Data.List as L
import Network.AWS.S3.Cache.Local
import System.FilePath (isDrive, joinPath)
import Test.Hspec
import Test.QuickCheck

newtype FilePathSplit = FilePathSplit [FilePath]

instance Show FilePathSplit where
  show (FilePathSplit fps) = joinPath fps

root :: FilePath
root = if isDrive "/" then "/" else "C:\\"

instance Arbitrary FilePathSplit where
  arbitrary = do
    Positive (Small depth) <- arbitrary
    FilePathSplit <$> vectorOf depth (sublistOf (['a'..'g'] ++ ['A'..'G']))

prop_subpathsRemoval :: FilePathSplit -> FilePathSplit -> Property
prop_subpathsRemoval (FilePathSplit fps1) (FilePathSplit fps2) =
  removeSubpaths (joinPath fps1withRoot : [joinPath (fps1withRoot ++ fps2)]) ===
  [joinPath fps1withRoot] .&&.
  removeSubpaths (joinPath (fps2withRoot ++ fps1) : [joinPath fps2withRoot]) ===
  [joinPath fps2withRoot]
  where
    fps1withRoot = root : fps1
    fps2withRoot = root : fps2

prop_seblingPathsKeeping :: FilePathSplit -> FilePathSplit -> Property
prop_seblingPathsKeeping (FilePathSplit fps1) (FilePathSplit fps2) =
  removeSubpaths nonSubpaths1 === L.sort nonSubpaths1 .&&.
  removeSubpaths nonSubpaths2 === L.sort nonSubpaths2
  where
    nonSubpaths1 = joinPath (fps1withRoot ++ ["foo"]) : [joinPath (fps1withRoot ++ fps2)]
    nonSubpaths2 = joinPath (fps1modWithRoot) : [joinPath (fps1withRoot ++ fps2)]
    fps1withRoot = root : fps1
    fps1modWithRoot = root : (init fps1 ++ [last fps1 ++ "foo"])


spec :: Spec
spec = do
  describe "Local file collection and archiving" $ do
    it "Removal of duplicate subpaths" $ property prop_subpathsRemoval
    it "Keeping of sebling paths" $ property prop_seblingPathsKeeping
    it "Unit test for discovered issue #9" $ do
      removeSubpaths [".ghc", ".ghcjs"] `shouldBe` [".ghc", ".ghcjs"]
      removeSubpaths [".ghcjs", ".ghc"] `shouldBe` [".ghc", ".ghcjs"]
      removeSubpaths [".ghc", ".ghcjs", ".ghc"] `shouldBe` [".ghc", ".ghcjs"]
