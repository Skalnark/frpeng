{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_frpeng (
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

bindir     = "/home/skalnark/.cabal/bin"
libdir     = "/home/skalnark/.cabal/lib/x86_64-linux-ghc-8.4.4/frpeng-0.1.0.0-GjXdfMOZGeXIsai0fjkQK9"
dynlibdir  = "/home/skalnark/.cabal/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/skalnark/.cabal/share/x86_64-linux-ghc-8.4.4/frpeng-0.1.0.0"
libexecdir = "/home/skalnark/.cabal/libexec/x86_64-linux-ghc-8.4.4/frpeng-0.1.0.0"
sysconfdir = "/home/skalnark/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "frpeng_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "frpeng_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "frpeng_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "frpeng_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "frpeng_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "frpeng_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
