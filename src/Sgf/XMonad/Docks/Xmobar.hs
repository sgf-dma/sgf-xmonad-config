{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.XMonad.Docks.Xmobar
    ( XmobarPID (..)
    , xmobarPP'
    )
  where

import Data.Monoid
import Control.Monad.State
import System.IO
import System.Posix.Types (ProcessID)

import XMonad
import XMonad.Hooks.DynamicLog

import Sgf.Data.List
import Sgf.XMonad.Util.Run (spawnPipe')
import Sgf.XMonad.Restartable
import Sgf.XMonad.Docks

-- This XmobarPID definition suitable for launching several xmobars. They will
-- be distinguished by config file name.
data XmobarPID      = XmobarPID
-- FIXME: Remove Monoid from xmobarPID .
                        { xmobarPID     :: First ProcessID
                        , xmobarConf    :: FilePath
-- FIXME: Rename xmobarPP2.
                        , xmobarPP2     :: Maybe PP
                        , xmobarToggle  :: Maybe (ButtonMask, KeySym)
                        }
  deriving (Typeable)
-- Show and Read instances, which omit Handle.
instance Show XmobarPID where
    showsPrec d x   = showParen (d > app_prec) $
        showString "XmobarPID {xmobarPID = " . showsPrec d (xmobarPID x)
        . showString ", xmobarConf = " . showsPrec d (xmobarConf x)
        . showString ", xmobarToggle = " . showsPrec d (xmobarToggle x)
        . showString "}"
      where
        app_prec    = 10
instance Read XmobarPID where
    readsPrec d     = readParen (d > app_prec) . runStateT $ do
        readLexsM ["XmobarPID"]
        xp <- readLexsM ["{", "xmobarPID", "="] >> readsPrecM d
        xc <- readLexsM [",", "xmobarConf", "="] >> readsPrecM d
        xt <- readLexsM [",", "xmobarToggle", "="] >> readsPrecM d
        readLexsM ["}"]
        let x = XmobarPID
                  { xmobarPID       = xp
                  , xmobarConf      = xc
                  , xmobarPP2       = Nothing
                  , xmobarToggle    = xt
                  }
        return x
      where
        app_prec    = 10

instance Eq XmobarPID where
    XmobarPID {xmobarConf = xcf} == XmobarPID {xmobarConf = ycf}
      | xcf == ycf  = True
      | otherwise   = False
-- FIXME: Remove Monoid.
instance Monoid XmobarPID where
    mempty          = XmobarPID
                        { xmobarPID = First Nothing
                        , xmobarConf = ""
                        , xmobarPP2  = Nothing
                        , xmobarToggle = Nothing
                        }
    x `mappend` y   = x{xmobarPID = xmobarPID x `mappend` xmobarPID y}
instance ProcessClass XmobarPID where
    getPidP         = getFirst . xmobarPID
    setPidP mp' x   = x{xmobarPID = First mp'}
instance RestartClass XmobarPID where
    runP x@(XmobarPID{xmobarConf = xcf, xmobarPP2 = Just xpp}) = do
        (h, p) <- spawnPipe' "xmobar" [xcf]
        return (x{ xmobarPID = First (Just p)
                 , xmobarPP2 = Just (xpp{ppOutput = hPutStrLn h})
                 })
    runP x@(XmobarPID{xmobarConf = xcf})
      | otherwise   = defaultRunP "xmobar" [xcf] x
    killP           = return . resetPipe <=< defaultKillP
      where
        resetPipe :: XmobarPID -> XmobarPID
        resetPipe x@(XmobarPID{xmobarPP2 = Just xpp}) =
            x{xmobarPP2 = Just (xpp{ppOutput = const (return ())})}
        resetPipe x = x
-- FIXME: Repeats resetPipe.
-- FIXME: Provide emptyXmobar value (like mempty).
-- FIXME: Use Lenses.
xmobarPP' :: PP
xmobarPP' = xmobarPP {ppOutput = const (return ())}
instance DockClass XmobarPID where
    dockToggleKey   = xmobarToggle
    getDockPP       = xmobarPP2
    setDockPP pp x  = maybe x (\t -> x{xmobarPP2 = Just t}) pp


-- Copy from xmobar/src/Config.hs . I need to read xmobar config for
-- determining its position.


-- End copy from xmobar/src/Config.hs .

