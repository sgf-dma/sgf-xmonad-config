{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Restartable
    ( ProcessClass (..)
    , withProcess
    , ListP
    , emptyListP
    , processList
    , RestartClass (..)
    , defaultRunP
    , startP
    , startP'
    , stopP
    , stopP'
    , restartP
    , restartP'
    )
  where

import Control.Monad
import Control.Monad.Trans
import Control.Exception (try, IOException)
import System.Posix.Process (getProcessPriority)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Posix.Types (ProcessID)

import XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.XMonad.Util.Run


-- To avoid orphan (ExtensionClass [a]) instance, i need newtype.
newtype ListP a     = ListP {_processList :: [a]}
  deriving (Show, Read, Typeable)
emptyListP :: ListP a
emptyListP          = ListP []
processList :: Lens (ListP a) [a]
processList f (ListP xs)    = fmap ListP (f xs)

instance (Show a, Read a, Typeable a) => ExtensionClass (ListP a) where
    initialValue    = emptyListP
    extensionType   = PersistentExtension

-- Strictly, all ProcessClass requirments are not required to define its
-- instance. But ProcessClass has these requirments, because i need
-- withProcess to work on its instances.
class (Eq a, Show a, Read a, Typeable a) => ProcessClass a where
    getPidP           :: a -> Maybe ProcessID
    setPidP           :: Maybe ProcessID -> a -> a

-- Run function on processes equal to given one. If there is no such processes
-- in extensible state, add given process there and run function on it.
withProcess :: ProcessClass a => (a -> X a) -> a -> X ()
withProcess f y     = do
    x  <- XS.get
    ps <- mapWhenM (== y) f (insertUniq y (view processList x))
    XS.put (set processList ps x)

class ProcessClass a => RestartClass a where
    runP              :: a -> X a
    -- restartP3' relies on PID 'Nothing' after killP, because it then calls
    -- startP3' and it won't do anything, if PID will still exist. So, here i
    -- should either set it to Nothing, or wait until it really terminates.
    killP             :: a -> X a
    killP x           = io $ do
                        whenJust (getPidP x) $ signalProcess sigTERM
                        return (setPidP Nothing x)

defaultRunP :: ProcessClass a => FilePath -> [String] -> a -> X a
defaultRunP x xs z  = do
                        p <- spawnPID' x xs
                        return (setPidP (Just p) z)

-- Based on doesPidProgRun .
refreshPid :: (MonadIO m, ProcessClass a) => a -> m a
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
stopP :: RestartClass a => a -> X ()
stopP               = withProcess stopP'

startP :: RestartClass a => a -> X ()
startP              = withProcess startP'

restartP :: RestartClass a => a -> X ()
restartP            = withProcess restartP'

