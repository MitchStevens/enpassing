{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_music (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mitch/.cabal/bin"
libdir     = "/home/mitch/.cabal/lib/x86_64-linux-ghc-8.2.2/music-0.1.0.0-1p9kCYwbP1yIUsJs8lnJR8-tests"
dynlibdir  = "/home/mitch/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/mitch/.cabal/share/x86_64-linux-ghc-8.2.2/music-0.1.0.0"
libexecdir = "/home/mitch/.cabal/libexec/x86_64-linux-ghc-8.2.2/music-0.1.0.0"
sysconfdir = "/home/mitch/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "music_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "music_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "music_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "music_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "music_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "music_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
