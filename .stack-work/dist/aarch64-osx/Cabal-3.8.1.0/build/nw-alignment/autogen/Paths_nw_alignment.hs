{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_nw_alignment (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/philliple/Documents/nw-alignment/.stack-work/install/aarch64-osx/64120596c0541fc1c70ce45179640ef01f4cc383467338a7ff74b762d4b9cf65/9.4.8/bin"
libdir     = "/Users/philliple/Documents/nw-alignment/.stack-work/install/aarch64-osx/64120596c0541fc1c70ce45179640ef01f4cc383467338a7ff74b762d4b9cf65/9.4.8/lib/aarch64-osx-ghc-9.4.8/nw-alignment-0.1.0.0-EEbqGWWp8hFA51CgyTHh9n-nw-alignment"
dynlibdir  = "/Users/philliple/Documents/nw-alignment/.stack-work/install/aarch64-osx/64120596c0541fc1c70ce45179640ef01f4cc383467338a7ff74b762d4b9cf65/9.4.8/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/philliple/Documents/nw-alignment/.stack-work/install/aarch64-osx/64120596c0541fc1c70ce45179640ef01f4cc383467338a7ff74b762d4b9cf65/9.4.8/share/aarch64-osx-ghc-9.4.8/nw-alignment-0.1.0.0"
libexecdir = "/Users/philliple/Documents/nw-alignment/.stack-work/install/aarch64-osx/64120596c0541fc1c70ce45179640ef01f4cc383467338a7ff74b762d4b9cf65/9.4.8/libexec/aarch64-osx-ghc-9.4.8/nw-alignment-0.1.0.0"
sysconfdir = "/Users/philliple/Documents/nw-alignment/.stack-work/install/aarch64-osx/64120596c0541fc1c70ce45179640ef01f4cc383467338a7ff74b762d4b9cf65/9.4.8/etc"

getBinDir     = catchIO (getEnv "nw_alignment_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "nw_alignment_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "nw_alignment_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "nw_alignment_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "nw_alignment_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "nw_alignment_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
