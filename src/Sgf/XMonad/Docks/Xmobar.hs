{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.XMonad.Docks.Xmobar
    ( Xmobar (..)
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

-- This Xmobar definition suitable for launching several xmobars. They will
-- be distinguished by config file name.
data Xmobar      = Xmobar
-- FIXME: Remove Monoid from xmobarPID .
                        { xmobarPID     :: First ProcessID
                        , xmobarConf    :: FilePath
-- FIXME: Rename xmobarPP2.
                        , xmobarPP2     :: Maybe PP
                        , xmobarToggle  :: Maybe (ButtonMask, KeySym)
                        }
  deriving (Typeable)
-- Show and Read instances, which omit Handle.
instance Show Xmobar where
    showsPrec d x   = showParen (d > app_prec) $
        showString "Xmobar {xmobarPID = " . showsPrec d (xmobarPID x)
        . showString ", xmobarConf = " . showsPrec d (xmobarConf x)
        . showString ", xmobarToggle = " . showsPrec d (xmobarToggle x)
        . showString "}"
      where
        app_prec    = 10
instance Read Xmobar where
    readsPrec d     = readParen (d > app_prec) . runStateT $ do
        readLexsM ["Xmobar"]
        xp <- readLexsM ["{", "xmobarPID", "="] >> readsPrecM d
        xc <- readLexsM [",", "xmobarConf", "="] >> readsPrecM d
        xt <- readLexsM [",", "xmobarToggle", "="] >> readsPrecM d
        readLexsM ["}"]
        let x = Xmobar
                  { xmobarPID       = xp
                  , xmobarConf      = xc
                  , xmobarPP2       = Nothing
                  , xmobarToggle    = xt
                  }
        return x
      where
        app_prec    = 10

instance Eq Xmobar where
    Xmobar {xmobarConf = xcf} == Xmobar {xmobarConf = ycf}
      | xcf == ycf  = True
      | otherwise   = False
-- FIXME: Remove Monoid.
instance Monoid Xmobar where
    mempty          = Xmobar
                        { xmobarPID = First Nothing
                        , xmobarConf = ""
                        , xmobarPP2  = Nothing
                        , xmobarToggle = Nothing
                        }
    x `mappend` y   = x{xmobarPID = xmobarPID x `mappend` xmobarPID y}
instance ProcessClass Xmobar where
    getPidP         = getFirst . xmobarPID
    setPidP mp' x   = x{xmobarPID = First mp'}
instance RestartClass Xmobar where
    runP x@(Xmobar{xmobarConf = xcf, xmobarPP2 = Just xpp}) = do
        (h, p) <- spawnPipe' "xmobar" [xcf]
        return (x{ xmobarPID = First (Just p)
                 , xmobarPP2 = Just (xpp{ppOutput = hPutStrLn h})
                 })
    runP x@(Xmobar{xmobarConf = xcf})
      | otherwise   = defaultRunP "xmobar" [xcf] x
    killP           = return . resetPipe <=< defaultKillP
      where
        resetPipe :: Xmobar -> Xmobar
        resetPipe x@(Xmobar{xmobarPP2 = Just xpp}) =
            x{xmobarPP2 = Just (xpp{ppOutput = const (return ())})}
        resetPipe x = x
-- FIXME: Repeats resetPipe.
-- FIXME: Provide emptyXmobar value (like mempty).
-- FIXME: Use Lenses.
xmobarPP' :: PP
xmobarPP' = xmobarPP {ppOutput = const (return ())}
instance DockClass Xmobar where
    dockToggleKey   = xmobarToggle
    getDockPP       = xmobarPP2
    setDockPP pp x  = maybe x (\t -> x{xmobarPP2 = Just t}) pp


-- Copy from xmobar/src/Config.hs . I need to read xmobar config for
-- determining its position.


-- End copy from xmobar/src/Config.hs .

