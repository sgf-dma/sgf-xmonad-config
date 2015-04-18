{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.XMonad.Docks.Xmobar
    ( Xmobar
    , xmobarConf
    , xmobarPP
    , xmobarToggle
    , defaultXmobar
    , xmobarsList
    )
  where

import Prelude hiding (catch)
import Control.Monad.State
import Control.Exception
import System.IO (hPutStrLn)
import System.FilePath
import System.Directory (getHomeDirectory)
import System.Posix.Types (ProcessID)

import XMonad
import qualified XMonad.Hooks.DynamicLog as L

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.Control.Exception
import Sgf.XMonad.Util.Run (spawnPipe')
import Sgf.XMonad.Restartable
import Sgf.XMonad.Docks

-- Ignore anything going to ppOutput.
resetPipe :: L.PP -> L.PP
resetPipe           = setA ppOutputL (const (return ()))

-- Redefine default xmobarPP to ignore all output. I need this to avoid
-- outputting status information (by dockLog and dynamicLogWithPP) to
-- ~/.xsession-errors (where xmonad's stdout is connected) until corresponding
-- xmobar process is started and RestartClass's Xmobar instance will
-- initialize ppOutput with pipe connected to that process's stdin.
defaultXmobarPP :: L.PP
defaultXmobarPP     = resetPipe L.xmobarPP

-- This Xmobar definition suitable for launching several xmobars. They will
-- be distinguished by config file name.
data Xmobar      = Xmobar
                        { _xmobarPid     :: Maybe ProcessID
                        , _xmobarConf    :: FilePath
                        , _xmobarPP      :: Maybe L.PP
                        , _xmobarToggle  :: Maybe (ButtonMask, KeySym)
                        }
  deriving (Typeable)
xmobarPid :: LensA Xmobar (Maybe ProcessID)
xmobarPid f z@(Xmobar {_xmobarPid = x})
                    = fmap (\x' -> z{_xmobarPid = x'}) (f x)
xmobarConf :: LensA Xmobar FilePath
xmobarConf f z@(Xmobar {_xmobarConf = x})
                    = fmap (\x' -> z{_xmobarConf = x'}) (f x)
xmobarPP :: LensA Xmobar (Maybe L.PP)
xmobarPP f z@(Xmobar {_xmobarPP = x})
                    = fmap (\x' -> z{_xmobarPP = x'}) (f x)
xmobarToggle :: LensA Xmobar (Maybe (ButtonMask, KeySym))
xmobarToggle f z@(Xmobar {_xmobarToggle = x})
                    = fmap (\x' -> z{_xmobarToggle = x'}) (f x)

-- Users creating Xmobar values (describing xmobar processes to start) should
-- overwrite defaultXmobar records through Lenses (PP lenses provided by
-- XMonad.Docks). Particularly, this ensures, that ppOutput will be set to
-- ignore output untill Xmobar's startP starts xmobar process and initializes
-- it with corresponding pipe. Otherwise, status information may be output to
-- ~/.xsession-errors (where xmonad's stdout will go).
defaultXmobar :: Xmobar
defaultXmobar       = Xmobar
                        { _xmobarPid    = Nothing
                        , _xmobarConf   = ".xmobarrc"
                        , _xmobarPP     = Just defaultXmobarPP
                        , _xmobarToggle = Nothing
                        }

-- Show and Read instances omiting some non-showable/non-readable records.
instance Show Xmobar where
    showsPrec d x   = showParen (d > app_prec) $
        showString "Xmobar {_xmobarPid = "  . showsPrec d (viewA xmobarPid x)
        . showString ", _xmobarConf = "     . showsPrec d (viewA xmobarConf x)
        . showString ", _xmobarToggle = "   . showsPrec d (viewA xmobarToggle x)
        . showString "}"
      where
        app_prec    = 10
instance Read Xmobar where
    readsPrec d     = readParen (d > app_prec) . runStateT $ do
        readLexsM ["Xmobar"]
        xp <- readLexsM ["{", "_xmobarPid", "="] >> readsPrecM d
        xc <- readLexsM [",", "_xmobarConf", "="] >> readsPrecM d
        xt <- readLexsM [",", "_xmobarToggle", "="] >> readsPrecM d
        readLexsM ["}"]
        -- The same as above: i need to overwrite records of defaultXmobar
        -- here, so right after reading saved extensible state ppOutput will
        -- be set to ignore output, until xmobar process will be restarted.
        let x = setA xmobarPid xp
                  . setA xmobarConf xc
                  . setA xmobarToggle xt
                  $ defaultXmobar
        return x
      where
        app_prec    = 10

instance Eq Xmobar where
    x == y
      | viewA xmobarConf x == viewA xmobarConf y = True
      | otherwise   = False
instance ProcessClass Xmobar where
    pidL            = xmobarPid
instance RestartClass Xmobar where
    runP x          = userCodeDef x $ do
        xcf <- absXmobarConf
        liftIO $ (doesFileExist' xcf) `catch` (throw . XmobarConfException)
        case (viewA xmobarPP x) of
          Just _ -> do
            (h, p) <- spawnPipe' "xmobar" [xcf]
            return
              . setA xmobarPid (Just p)
              . setA (xmobarPP . maybeL . ppOutputL) (hPutStrLn h)
              $ x
          Nothing  -> do
            defaultRunP "xmobar" [xcf] x
      where
        -- If xmobarConf is relative, take it from home directory, not from
        -- current directory.
        absXmobarConf :: MonadIO m => m FilePath
        absXmobarConf   = liftIO $ do
          d <- getHomeDirectory
          let cf = viewA xmobarConf x
          if (isRelative cf) then return (d </> cf) else return cf
    -- I need to reset pipe (to ignore output), because though process got
    -- killed, xmobar value still live in Extensible state and dockLog does
    -- not check process existence - just logs according to PP, if any.
    killP           = defaultKillP . modifyA (xmobarPP . maybeL) resetPipe
instance DockClass Xmobar where
    dockToggleKey   = viewA xmobarToggle
    ppL             = xmobarPP

data XmobarException    = XmobarConfException FileException
  deriving (Typeable)
instance Show XmobarException where
    show (XmobarConfException x) = "Xmobar config: " ++ show x
instance Exception XmobarException where

-- Lens for obtaining list of all Xmobars stored in extensible state.
xmobarsList :: LensA (ListP Xmobar) [Xmobar]
xmobarsList         = processList

