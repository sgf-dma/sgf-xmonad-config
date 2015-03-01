
import XMonad
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Hooks.DynamicLog

import Graphics.X11.ExtraTypes.XF86 -- For media keys.
import qualified Data.Map as M
import Control.Applicative
import System.FilePath ((</>))
import System.Process

import Data.Monoid

import Sgf.XMonad.Docks
import Sgf.XMonad.Docks.Xmobar
import Sgf.XMonad.Docks.Trayer
import Sgf.Control.Lens
import Sgf.XMonad.Restartable

main :: IO ()
main                = do
    -- FIXME: Spawn process directly, not through shell.
    xmonad
      . handleDocks [(0, xK_b)] [xmobarTop, xmobarBot]
      . alterKeys myKeys
      $ defaultConfig
          { layoutHook = layout
          -- Workspace "lock" is for xtrlock only and it is inaccessible for
          -- workspace switch keys.
          , workspaces = map show [1..9] ++ ["lock"]
          , modMask = mod4Mask
          , focusFollowsMouse = False
          , terminal = "xterm -fg black -bg white"
	  , logHook = traceXS "Huh"
          --, layoutHook = smartBorders $ layoutHook xfceConfig
          }


traceXS :: String -> X ()
traceXS l = do
    trace l
    xs <- XS.get
    mapM_ (trace . show) (view processList xs :: [Xmobar])
    ts <- XS.get
    mapM_ (trace . show) (view processList ts :: [Trayer])
    --fs <- XS.get
    --mapM_ (trace . show) (fs :: [FehPID])
    {-
    trace "For trayer:"
    forM_ ts $ \x -> whenJust (getPidP x) pidStatus
    trace "For feh:"
    forM_ fs $ \x -> whenJust (getPidP x) pidStatus-}
  --where
  --  pidStatus :: ProcessID -> X ()
  --  pidStatus p = io (getProcessStatus False False p)  >>= trace . show

xmobarTop :: Xmobar
xmobarTop     = Xmobar
                  { xmobarPID = First Nothing
                  , xmobarConf = "/home/sgf" </> ".xmobarrc"
                  , xmobarPP2 = Just xmobarPP'
                        { ppTitle  = xmobarColor "green" "" . shorten 50
                        }
                  , xmobarToggle = Just (shiftMask, xK_v)
                  }

xmobarTop2 :: Xmobar
xmobarTop2    = Xmobar
                  { xmobarPID = First Nothing
                  , xmobarConf = "/home/sgf" </> ".xmobarrc"
                  , xmobarPP2 = Nothing
                  , xmobarToggle = Nothing
                  }

xmobarBot :: Xmobar
xmobarBot     = Xmobar
                  { xmobarPID = First Nothing
                  , xmobarConf = "/home/sgf" </> ".xmobarrc2"
                  , xmobarPP2 = Just xmobarPP'
                        { ppTitle  = xmobarColor "red" "" . shorten 50
                        }
                  , xmobarToggle = Just (shiftMask, xK_b)
                  }

-- Layouts definition from defaultConfig with Full layout without borders.
layout = tiled ||| Mirror tiled ||| noBorders Full
  where	
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100


-- Union my keys config with current one in ((->) XConfig Layout) applicative
-- functor. Union prefers left argument, when duplicate keys are found, thus
-- my should go first.
alterKeys :: (XConfig Layout -> M.Map (ButtonMask, KeySym) (X ()))
             -> XConfig l -> XConfig l
alterKeys myKs cf@(XConfig {keys = ks}) = cf {keys = M.union <$> myKs <*> ks}

-- FIXME: When i wait for xtrlock process to terminate, i always come back to
-- old workpace, where i was before pressing lock keys (regardless of
-- workpspace switching code) and all windows are closed on it. Why? But it
-- does not close windows, if i comment out code, obtaining current
-- workspace..

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

-- My key bindings. They are intended for use with alterKeys only.
myKeys :: XConfig l -> M.Map (ButtonMask, KeySym) (X ())
myKeys (XConfig {modMask = m}) =
    M.fromList
      [ 
      --((m .|. shiftMask, xK_p), spawn "exec gmrun")
        ((m .|. shiftMask, xK_z), lock)
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0,           xK_Print), spawn "scrot")
      , ((m,           xK_v), stopP xmobarBot)

      -- Audio keys.
      , ((0,     xF86XK_AudioLowerVolume), spawn "amixer set Master 1311-")
      -- FIXME: This really not exactly what i want. I want, that if sound is
      -- muted, one VolUp only unmutes it. Otherwise, just VolUp-s.
      , ((0,     xF86XK_AudioRaiseVolume), spawn $ "amixer set Master unmute; "
          					 ++ "amixer set Master 1311+")
      , ((0,     xF86XK_AudioMute       ), spawn "amixer set Master mute")
      ]

