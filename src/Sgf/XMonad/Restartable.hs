{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Restartable
    ( RestartClass (..)
    , defaultRunP
    , startP
    , startP'
    , stopP
    , stopP'
    , restartP
    , restartP'
    )
  where

import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Exception (try, IOException)
import System.Posix.Process (getProcessPriority)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Posix.Types (ProcessID)

import XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS

import Sgf.Data.List
import Sgf.XMonad.Util.Run

class (Eq a, Monoid a) => RestartClass a where
    getPidP           :: a -> Maybe ProcessID
    setPidP           :: Maybe ProcessID -> a -> a
    runP              :: a -> X a
    -- restartP3' relies on PID 'Nothing' after killP, because it then calls
    -- startP3' and it won't do anything, if PID will still exist. So, here i
    -- should either set it to Nothing, or wait until it really terminates.
    killP             :: a -> X a
    killP x           = io $ do
                        whenJust (getPidP x) $ signalProcess sigTERM
                        return (setPidP Nothing x)

defaultRunP :: RestartClass a => FilePath -> [String] -> a -> X a
defaultRunP x xs z  = do
                        p <- spawnPID' x xs
                        return (setPidP (Just p) z)

instance (Show a, Read a, Typeable a) => ExtensionClass [a] where
    initialValue    = []
    extensionType   = PersistentExtension

-- Run function on matched PIDs with specific type.  Argument's Eq instance is
-- used to find value in extensible state to act upon. Also, argument is
-- `mappend`-ed to found value, so i should pass mempty, if i want to just
-- "match", and something different, if i want to "match and replace".
runWithP :: (Eq a, Monoid a, ExtensionClass [a]) => (a -> X a) -> a -> X ()
runWithP f y        = do
    xs  <- XS.gets (insertWith mappend y)
    xs' <- mapM (\x -> if y == x then f x else return x) xs
    XS.put xs'

-- Based on doesPidProgRun .
refreshPid :: (MonadIO m, RestartClass a) => a -> m a
refreshPid x        = case (getPidP x) of
    Nothing -> return x
    Just p  -> liftIO $ do
      either (const (setPidP Nothing x)) (const x)
      `fmap` (try $ getProcessPriority p :: IO (Either IOException Int))

-- Here are versions of start/stop working on argument, not extensible state.
-- Run, if program is not running or already dead, otherwise do nothing.
startP' :: RestartClass a => a -> X a
startP' x           = do
  x' <- refreshPid x
  case (getPidP x') of
    Nothing   -> runP x'
    Just _    -> return x'

-- Stop program.
stopP' :: RestartClass a => a -> X a
stopP'              = killP <=< refreshPid

-- Stop program and run again. Note, that it will run again only, if killP
-- kills it properly: either sets pid to Nothing or waits until it dies,
-- because startP' checks whether program is running.
restartP' :: RestartClass a => a -> X a
restartP'           = startP' <=< stopP'

-- Here are versions of start/restart working on extensible state.
-- Usually, these should be used.
stopP :: (ExtensionClass [a], RestartClass a) => a -> X ()
stopP               = runWithP stopP'

startP :: (ExtensionClass [a], RestartClass a) => a -> X ()
startP              = runWithP startP'

restartP :: (ExtensionClass [a], RestartClass a) => a -> X ()
restartP            = runWithP restartP'

