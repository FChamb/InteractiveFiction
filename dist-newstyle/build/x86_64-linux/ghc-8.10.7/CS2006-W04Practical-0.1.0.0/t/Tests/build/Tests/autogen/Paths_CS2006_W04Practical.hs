{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_CS2006_W04Practical (
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

bindir     = "/home/fc84/.cabal/bin"
libdir     = "/home/fc84/.cabal/lib/x86_64-linux-ghc-8.10.7/CS2006-W04Practical-0.1.0.0-inplace-Tests"
dynlibdir  = "/home/fc84/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/fc84/.cabal/share/x86_64-linux-ghc-8.10.7/CS2006-W04Practical-0.1.0.0"
libexecdir = "/home/fc84/.cabal/libexec/x86_64-linux-ghc-8.10.7/CS2006-W04Practical-0.1.0.0"
sysconfdir = "/home/fc84/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CS2006_W04Practical_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CS2006_W04Practical_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "CS2006_W04Practical_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "CS2006_W04Practical_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CS2006_W04Practical_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CS2006_W04Practical_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
