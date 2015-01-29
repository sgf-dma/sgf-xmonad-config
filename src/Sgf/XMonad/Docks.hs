{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Docks
    ( handleDocks
    )
  where

import Data.Monoid
import qualified Data.Map as M
import Control.Monad
import Control.Applicative

import XMonad
import XMonad.Hooks.ManageDocks hiding (docksEventHook)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig (additionalKeys)

handleDocks :: LayoutClass l Window =>
               XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
handleDocks x = 
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

