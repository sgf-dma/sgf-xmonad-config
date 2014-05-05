
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Run (spawnPipe)

import Graphics.X11.ExtraTypes.XF86 -- For media keys.
import qualified Data.Map as M
import Control.Applicative
import System.IO (hPutStrLn)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Process

main :: IO ()
main                = do
    -- FIXME: Spawn process directly, not through shell.
    runFehBg
    _ <- runTrayer
    xmPipe <- spawnPipe ("xmobar ~/.xmobarrc")
    xmonad
      . alterKeys myKeys
      $ defaultConfig
          { manageHook = manageDocks <+> manageHook defaultConfig
          , layoutHook = avoidStruts $ layout
          , logHook    = dynamicLogWithPP xmobarPP
                           { ppOutput = hPutStrLn xmPipe
                           , ppTitle  = xmobarColor "green" ""
                                          . shorten 50
                           }
          , modMask = mod4Mask
          , focusFollowsMouse = False
          , terminal = "xterm -fg black -bg white"
          --, layoutHook = smartBorders $ layoutHook xfceConfig
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


-- Run trayer.
runTrayer :: IO ProcessHandle
runTrayer           = do
    let tray = proc "trayer"
                 [ "--edge", "top", "--align", "right"
                 , "--SetDockType", "true", "--SetPartialStrut", "true"
                 , "--expand", "true", "--width", "10"
                 , "--transparent", "true" , "--tint", "0x191970"
                 , "--height", "12"
                 ]
    (_, _, _, p) <- createProcess tray
    return p

-- Set up background image using feh.
runFehBg :: IO ProcessHandle
runFehBg           = do
    home <- getHomeDirectory
    fehBg <- readFile (home </> ".fehbg")
    (_, _, _, p) <- createProcess (shell fehBg)
    return p

-- Union my keys config with current one in ((->) XConfig Layout) applicative
-- functor. Union prefers left argument, when duplicate keys are found, thus
-- my should go first.
alterKeys :: (XConfig Layout -> M.Map (ButtonMask, KeySym) (X ()))
             -> XConfig l -> XConfig l
alterKeys myKs cf@(XConfig {keys = ks}) = cf {keys = M.union <$> myKs <*> ks}

-- My key bindings. They are intended for use with alterKeys only.
myKeys :: XConfig l -> M.Map (ButtonMask, KeySym) (X ())
myKeys (XConfig {modMask = m}) =
    M.fromList
      [ 
      --((m .|. shiftMask, xK_p), spawn "exec gmrun")
        ((m .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0,           xK_Print), spawn "scrot")

      -- Audio keys.
      , ((0,     xF86XK_AudioLowerVolume), spawn "amixer set Master 1311-")
      -- FIXME: This really not exactly what i want. I want, that if sound is
      -- muted, one VolUp only unmutes it. Otherwise, just VolUp-s.
      , ((0,     xF86XK_AudioRaiseVolume), spawn $ "amixer set Master unmute; "
          					 ++ "amixer set Master 1311+")
      , ((0,     xF86XK_AudioMute       ), spawn "amixer set Master mute")
      ]

