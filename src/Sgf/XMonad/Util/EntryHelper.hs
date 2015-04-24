
module Sgf.XMonad.Util.EntryHelper
    ( withHelper
    )
  where

import System.FilePath  ((</>))
import System.Directory (getHomeDirectory, copyFile)
import System.Exit

import XMonad
import qualified XMonad.Util.EntryHelper as EH


-- Redefine withHelper to use cabal for building xmonad .
withHelper :: IO () -> IO ()
withHelper e        = EH.withCustomHelper EH.defaultConfig
                        { EH.run = e
                        , EH.compile = cabalCompile
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

