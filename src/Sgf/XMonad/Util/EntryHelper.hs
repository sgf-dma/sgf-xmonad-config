{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Util.EntryHelper
    ( withHelper
    )
  where

import Control.Exception
import System.FilePath  ((</>))
import System.Directory (getHomeDirectory, copyFile)
import System.Exit

import XMonad
import qualified XMonad.Util.EntryHelper as EH


-- Redefine withHelper to use cabal for building xmonad and throw an
-- exception, if lock file exists. Then '--recompile' will exit with non-zero
-- code and '--restart' won't run (with default 'mod+q' action). The exception
-- will be evaluated, because it's thrown in compile and postCompile will run
-- afterwards.
withHelper :: IO () -> IO ()
withHelper e        = EH.withCustomHelper EH.defaultConfig
                        { EH.run = e
                        , EH.compile = EH.withLock (throw lockAlreadyExists)
                                         . cabalCompile
                        , EH.postCompile = cabalInstall
                        }

cabalCompile :: Bool -> IO ExitCode
cabalCompile force  = do
    let buildCmd = "cabal configure; cabal build"
        cmd      = (if force then "cabal clean; " else "") ++ buildCmd
    uninstallSignalHandlers
    r <- EH.compileUsingShell cmd
    installSignalHandlers
    return r

cabalInstall :: ExitCode -> IO ()
cabalInstall ExitSuccess    = do
    hd <- getHomeDirectory
    xd <- getXMonadDir
    copyFile (xd </> "dist/build/xmonad/xmonad") (hd </> "bin/xmonad")
cabalInstall r              = EH.defaultPostCompile r

-- "Lock file already exists" exception. It may be useful, because 'mod+q'
-- executes (by default) `xmonad --recompile && xmonad --restart` using shell,
-- and if recompile fails due to existing lock, i need non-zero exit code to
-- prevent restart from running. And the only way to get non-zero exit code is
-- to throw an exception in default value returned by withLock (i need to
-- actually evaluate withLock's result (IO a) to make it actually happen).
data LockAlreadyExists  = LockAlreadyExists FilePath
  deriving (Typeable)
-- Default value.
lockAlreadyExists :: LockAlreadyExists
lockAlreadyExists   = LockAlreadyExists ""
instance Show LockAlreadyExists where
    show (LockAlreadyExists f)
      | null f      = "Lock file already exists."
      | otherwise   = "Lock file " ++ f ++ " already exists."
instance Exception LockAlreadyExists where

