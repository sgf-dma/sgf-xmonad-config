{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Session
    ( Feh
    , defaultFeh
    , fehsList
    )
  where

import System.FilePath
import System.Directory
import System.Posix.Types (ProcessID)

import XMonad

import Sgf.Control.Lens
import Sgf.XMonad.Restartable

newtype Feh         = Feh {_fehPid  :: Maybe ProcessID}
  deriving (Show, Read, Typeable)
fehPid :: LensA Feh (Maybe ProcessID)
fehPid f z@(Feh {_fehPid = x})
                    = fmap (\x' -> z{_fehPid = x'}) (f x)
defaultFeh :: Feh
defaultFeh          = Feh {_fehPid = Nothing}

instance Eq Feh where
  _ == _    = True
-- FIXME: Open only one pipe!
instance ProcessClass Feh where
    pidL            = fehPid
instance RestartClass Feh where
    runP x          = do
        cmd <- liftIO $ do
          h <- getHomeDirectory
          let f = h </> ".fehbg"
          b <- doesFileExist f
          if b
            then readFile f
            else return "xsetroot -grey"
        -- ~/.fehbg content written assuming evaluation by shell,
        -- but i still need real process's PID, so add 'exec' .
        p <- spawnPID ("exec " ++ cmd)
        return (setA fehPid (Just p) x)

-- Lens for obtaining list of all Feh processes stored in extensible state.
fehsList :: LensA (ListP Feh) [Feh]
fehsList            = processList

