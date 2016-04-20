module Distribution.CurrentPackageDescription
  ( currentPackageDescription
  , getField
  ) where

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity

import Data.List           (isSuffixOf)
import Language.Haskell.TH (stringE, runIO, Q, Exp)
import System.Directory    (getCurrentDirectory, getDirectoryContents)


getField :: (PackageDescription -> String) -> Q Exp
getField f = runIO currentPackageDescription >>= stringE . f

currentPackageDescription :: IO PackageDescription
currentPackageDescription = fmap packageDescription $ do
  dir <- getCurrentDirectory
  cs <- cabalFiles dir
  case cs of
    (c:_) -> readPackageDescription silent c
    [] -> error $ "Couldn't find a cabal file in the current working directory (" ++ dir ++ ")"

cabalFiles :: FilePath -> IO [FilePath]
cabalFiles dir = do
  files <- getDirectoryContents dir
  return $ filter (".cabal" `isSuffixOf`) files
