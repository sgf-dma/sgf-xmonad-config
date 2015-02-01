{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.XMonad.Docks
    ( handleDocks
    , XmobarPID (..)
    , TrayerPID (..)
    )
  where

import Data.List
import Data.Monoid
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import System.IO
import System.Posix.Types (ProcessID)

import XMonad
import XMonad.Hooks.ManageDocks hiding (docksEventHook)
import XMonad.Hooks.DynamicLog
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.Util.ExtensibleState as XS

import Sgf.XMonad.Util.Run (spawnPipe')
import Sgf.Data.List (readLexs)
import Sgf.XMonad.Restartable

handleDocks :: LayoutClass l Window =>
               XmobarPID -> XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
handleDocks xm x = 
    additionalKeys <*> (sequence [toggleDocks, toggleBotDock]) $ x
      -- First, demanage dock applications.
      { manageHook = manageDocks <+> manageHook x
      -- Then refresh screens after new dock appears.
      , handleEventHook = docksEventHook <+> handleEventHook x
      -- Reduce Rectangle available for other windows.
      , layoutHook = avoidStruts (layoutHook x)
      -- I can union keys explicitly
      --, keys = sequence [toggleBotDock, toggleDocks, keys x] >>=
      --    return . foldr M.union M.empty
      -- or use additionalKeys above.

      -- Log to all open xmobar pipes.
      , logHook = xmobarLog >> logHook x
      , startupHook = restartP xm >> startupHook x
      }

-- docksEventHook version from xmobar tutorial (5.3.1 "Example for using the
-- DBus IPC interface with XMonad"), which refreshes screen on unmap events as
-- well.
docksEventHook :: Event -> X All
docksEventHook e = do
    when (et == mapNotify || et == unmapNotify) $
        whenX ((not `fmap` (isClient w)) <&&> runQuery checkDock w) refresh
    return (All True)
    where w  = ev_window e
          et = ev_event_type e

toggleDocks :: XConfig l -> ((ButtonMask, KeySym), X())
toggleDocks XConfig {modMask = m} =
    ((m, xK_b), sendMessage ToggleStruts)

toggleBotDock :: XConfig l -> ((ButtonMask, KeySym), X())
toggleBotDock XConfig {modMask = m} =
    ((m .|. shiftMask, xK_b), sendMessage (ToggleStrut D))


xmobarLog :: X()
xmobarLog           = do
    xs <- XS.get
    forM_ xs $ \XmobarPID {xmobarPipe = (_, mp)} ->
      whenJust mp $ \xmPipe ->
        dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmPipe
          , ppTitle  = xmobarColor "green" "" . shorten 50
          }


-- This XmobarPID definition suitable for launching several xmobars. They will
-- be distinguished by config file name.
data XmobarPID      = XmobarPID
                        { xmobarPID  :: First ProcessID
                        , xmobarConf :: FilePath
                        , xmobarPipe :: (Bool, Maybe Handle)
                        }
  deriving (Typeable)
-- Show and Read instances, which omit Handle.
instance Show XmobarPID where
    showsPrec d x   = showParen (d > app_prec) $
        showString "XmobarPID {xmobarPID = " . showsPrec d (xmobarPID x)
        . showString ", xmobarConf = " . showsPrec d (xmobarConf x)
        . showString ", xmobarPipe = " . showsPrec d (fst $ xmobarPipe x)
        . showString "}"
      where
        app_prec    = 10
instance Read XmobarPID where
    readsPrec d     = readParen (d > app_prec) $ \r0 -> do
        (xp, r1) <- readsPrec d =<<
          readLexs ["XmobarPID", "{", "xmobarPID", "="] r0
        (xc, r2) <- readsPrec d =<<
          readLexs [",", "xmobarConf", "="] r1
        (xb, r3) <- readsPrec d =<<
          readLexs [",", "xmobarPipe", "="] r2
        ("}", r4) <- lex r3
        let x = XmobarPID
                  { xmobarPID   = xp
                  , xmobarConf  = xc
                  , xmobarPipe  = (xb, Nothing)
                  }
        return (x, r4)
      where
        app_prec    = 10

instance Eq XmobarPID where
    XmobarPID {xmobarConf = xcf} == XmobarPID {xmobarConf = ycf}
      | xcf == ycf  = True
      | otherwise   = False
instance Monoid XmobarPID where
    mempty          = XmobarPID
                        { xmobarPID = First Nothing
                        , xmobarConf = ""
                        , xmobarPipe = (False, Nothing)
                        }
    x `mappend` y   = x{xmobarPID = xmobarPID x `mappend` xmobarPID y}
instance RestartClass XmobarPID where
    getPidP         = getFirst . xmobarPID
    setPidP mp' x   = x{xmobarPID = First mp'}
    runP x@(XmobarPID{xmobarConf = xcf, xmobarPipe = (xb, _)})
      | xb          = do
        (h, p) <- spawnPipe' "xmobar" [xcf]
        return (x{xmobarPID = First (Just p), xmobarPipe = (True, Just h)})
      | otherwise   = defaultRunP "xmobar" [xcf] x

-- Copy from xmobar/src/Config.hs . I need to read xmobar config for
-- determining its position.


-- End copy from xmobar/src/Config.hs .

-- This TrayerPID definition allows to run only one trayer instance, bceause
-- all values of this type are equal.
newtype TrayerPID    = TrayerPID {trayerPID  :: First ProcessID}
  deriving (Show, Read, Typeable, Monoid)
instance Eq TrayerPID where
  _ == _    = True
instance RestartClass TrayerPID where
  getPidP        = getFirst . trayerPID
  setPidP mp' x  = x{trayerPID = First mp'}
  runP           = defaultRunP "trayer"
      [ "--edge", "top", "--align", "right"
      , "--SetDockType", "true", "--SetPartialStrut", "true"
      , "--expand", "true", "--width", "10"
      , "--transparent", "true" , "--tint", "0x191970"
      , "--height", "12"
      ]

