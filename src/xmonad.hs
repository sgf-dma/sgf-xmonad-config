{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Hooks.DynamicLog (shorten, xmobarColor)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.LayoutScreens
import XMonad.Util.EZConfig (additionalKeys)

import Graphics.X11.ExtraTypes.XF86 -- For media keys.
import qualified Data.Map as M
import Control.Applicative
import System.Process

import Sgf.Control.Lens
import Sgf.XMonad.Restartable
import Sgf.XMonad.Docks
import Sgf.XMonad.Docks.Xmobar
import Sgf.XMonad.Docks.Trayer
import Sgf.XMonad.Session

main :: IO ()
main                = do
    -- FIXME: Spawn process directly, not through shell.
    xmonad
      . handleFullscreen
      . handleDocks (0, xK_b) myDocks
      . (additionalKeys <*> myKeys)
      $ defaultConfig
          {
          -- Workspace "lock" is for xtrlock only and it is inaccessible for
          -- workspace switch keys.
          workspaces = map show [1..9] ++ ["lock"]
          , modMask = mod4Mask
          , focusFollowsMouse = False
          , terminal = "xterm -fg black -bg white"
          , logHook = traceXS "traceXS"
          , startupHook = restartP' feh >> return ()
          }

-- Modify layoutHook to remove borders around floating windows covering whole
-- screen and around tiled windows in non-ambiguous cases. Also, add event
-- hook to detect windows going to fullscreen using _NET_WM_STATE protocol
-- (EWMH).
handleFullscreen :: LayoutClass l Window => XConfig l
                    -> XConfig (ModifiedLayout (ConfigurableBorder Ambiguity) l)
handleFullscreen cf = cf
    { layoutHook        = lessBorders OtherIndicated (layoutHook cf)
    , handleEventHook   = fullscreenEventHook <+> handleEventHook cf
    }

myDocks :: LayoutClass l Window => [DockConfig l]
myDocks     = addDock trayer : map addDock [xmobarTop, xmobarBot]

xmobarTop :: Xmobar
xmobarTop           = setA (xmobarPP . maybeL . ppTitleL) t
                        $ defaultXmobar
  where
    t :: String -> String
    t               = xmobarColor "green" "" . shorten 50
xmobarBot :: Xmobar
xmobarBot     = setA xmobarConf (".xmobarrc2")
                  . setA (xmobarPP . maybeL . ppTitleL) t
                  . setA xmobarToggle (Just (shiftMask, xK_b))
                  $ defaultXmobar
   where
    t :: String -> String
    t               = xmobarColor "red" "" . shorten 50
trayer :: Trayer
trayer              = defaultTrayer
feh :: Feh
feh                 = defaultFeh

traceXS :: String -> X ()
traceXS l = do
    withWindowSet $ \ws -> do
      whenJust (W.stack . W.workspace . W.current $ ws) $ \s -> do
        ts <- mapM (runQuery title) (W.integrate s)
        trace "Tiled:"
        trace (show ts)
      trace "Floating:"
      fs <- mapM (runQuery title) (M.keys . W.floating $ ws)
      trace (show fs)
    trace l
    xs <- XS.gets (viewA xmobarsList)
    mapM_ (trace . show) xs
    ts <- XS.gets (viewA trayersList)
    mapM_ (trace . show) ts
    fs <- XS.gets (viewA fehsList)
    mapM_ (trace . show) fs

-- Key for hiding all docks defined by handleDocks, keys for hiding particular
-- dock, if any, defined in that dock definition (see above).
myKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
myKeys XConfig {modMask = m} =
      [ 
      --((m .|. shiftMask, xK_p), spawn "exec gmrun")
        ((m .|. shiftMask, xK_z), lock)
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0,           xK_Print), spawn "scrot")
      , ((m,           xK_n), stopP xmobarBot)
      , ((m .|. shiftMask, xK_n), startP xmobarBot)
      -- For testing two screens.
      , ((m .|. shiftMask,                 xK_space), layoutScreens 2 testTwoScreen)
      , ((m .|. controlMask .|. shiftMask, xK_space), rescreen)

      -- Audio keys.
      , ((0,     xF86XK_AudioLowerVolume), spawn "amixer set Master 1311-")
      -- FIXME: This really not exactly what i want. I want, that if sound is
      -- muted, one VolUp only unmutes it. Otherwise, just VolUp-s.
      , ((0,     xF86XK_AudioRaiseVolume), spawn $ "amixer set Master unmute; "
          					 ++ "amixer set Master 1311+")
      , ((0,     xF86XK_AudioMute       ), spawn "amixer set Master mute")
      ]

-- Two screens dimensions for layoutScreen. Two xmobars have height 17, total
-- resolution is 1680x1050 .
testTwoScreen :: FixedLayout a
testTwoScreen       = fixedLayout
                        [ Rectangle 0 17 1680 536
                        , Rectangle 0 553 1680 480
                        ]

-- FIXME: When i wait for xtrlock process to terminate, i always come back to
-- old workpace, where i was before pressing lock keys (regardless of
-- workpspace switching code) and all windows are closed on it. Why? But it
-- does not close windows, if i comment out code, obtaining current
-- workspace..
-- 
-- Get current workspace tag, then switch to workspace "lock" (dedicated for
-- "xtrlock" and inaccessible for workspace switch keys) and lock. After
-- unlocking return back to workspace, where i was before.
lock :: X ()
lock                = do
                        --wi <- gets curWsId
                        windows (W.greedyView "lock")
                        spawn "xtrlock"
                        --p <- liftIO xtrlock
                        --liftIO (waitForProcess p)
                        --windows (W.greedyView wi)
                        return ()
  where
    -- Get current workspace tag.
    curWsId :: XState -> WorkspaceId
    curWsId         = W.tag . W.workspace . W.current . windowset
    xtrlock :: IO ProcessHandle
    xtrlock         = do
                        (_, _, _, p) <-
                            createProcess (proc "/usr/bin/xtrlock" [])
                        return p

