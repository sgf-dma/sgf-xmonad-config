
module Sgf.XMonad.Util.Run
    ( spawnPipe'
    , spawnPID'
    )
  where

import XMonad.Core

import System.IO
import System.Posix.IO
import System.Posix.Process (executeFile)
import System.Posix.Types (ProcessID)
import Control.Monad.Trans

-- Variants of spawnPipe and spawnPID running process directly (not through
-- shell).
spawnPipe' :: FilePath -> [String] -> X (Handle, ProcessID)
spawnPipe' x xs     = io $ do
                        (rd, wr) <- createPipe
                        setFdOption wr CloseOnExec True
                        h <- fdToHandle wr
                        hSetBuffering h LineBuffering
                        p <- xfork $ do
                              _ <- dupTo rd stdInput
                              executeFile x True xs Nothing
                        closeFd rd
                        return (h, p)

spawnPID' :: MonadIO m => FilePath -> [String] -> m ProcessID
spawnPID' x xs      = xfork $ executeFile x True xs Nothing

