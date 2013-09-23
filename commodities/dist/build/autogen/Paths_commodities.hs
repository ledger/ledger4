module Paths_commodities (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Volumes/Data/Home/.cabal/bin"
libdir     = "/Volumes/Data/Home/.cabal/lib/x86_64-osx-ghc-7.6.3/commodities-0.1.0.0"
datadir    = "/Volumes/Data/Home/.cabal/share/x86_64-osx-ghc-7.6.3/commodities-0.1.0.0"
libexecdir = "/Volumes/Data/Home/.cabal/libexec"
sysconfdir = "/Volumes/Data/Home/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "commodities_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "commodities_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "commodities_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "commodities_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "commodities_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
