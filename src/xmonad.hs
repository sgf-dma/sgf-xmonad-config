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

import Data.Maybe
import Data.List
import Data.Monoid
import qualified Data.Foldable as F
import XMonad.Layout.IndependentScreens

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
          workspaces = withScreens 2 (map show [1..9] ++ ["lock"])
          --workspaces = map show [1..9] ++ ["lock"]
          , modMask = mod4Mask
          , focusFollowsMouse = False
          , terminal = "xterm -fg black -bg white"
          , logHook = traceXS "traceXS"
          , startupHook = restartP' feh >> return ()
          }


-- Generic.
-- Fold list of functions by function composition.
apList :: F.Foldable t => t (a -> a) -> a -> a
apList              = appEndo . F.foldMap Endo


-- StackSet operations.
-- Current ScreenId.
currentScreen :: WindowSet -> ScreenId
currentScreen       = W.screen . W.current

-- Last (greatest) ScreenId. Assume, that numbering of Screens by xmonad and
-- by IndependentScreens is the same - starting from 0.
lastScreen :: WindowSet -> ScreenId
lastScreen          = maximum . map W.screen . W.screens

-- All workspace tags.
workspaceTags :: WindowSet -> [WorkspaceId]
workspaceTags       = map W.tag . W.workspaces

-- All windows of specified workspace.
tagWindows :: WorkspaceId -> WindowSet -> [Window]
tagWindows i s      = fromMaybe [] $ do
    w <- find ((i ==) . W.tag) (W.workspaces s)
    return (W.integrate' (W.stack w))

-- Moves all specified windows to another workspace.
shiftAllWins :: WorkspaceId -> [Window] -> WindowSet -> WindowSet
--shiftAllWins to     = appEndo . F.foldMap (Endo . W.shiftWin to)
shiftAllWins to     = apList . map (W.shiftWin to)

-- Move all windows from one workspace to another.
shiftAll' :: WorkspaceId -> WorkspaceId -> WindowSet -> WindowSet
shiftAll' to from   = flip (shiftAllWins to) <*> tagWindows from

-- Move all windows from current workspace to another.
shiftAll :: WorkspaceId -> WindowSet -> WindowSet
shiftAll to         = flip (shiftAll' to) <*> W.currentTag


-- IndependentScreens operations.
-- Number of screens used by IndependentScreens (starting from 1).
allScreens :: WindowSet -> Int
allScreens          = length . nub . map unmarshallS . workspaceTags

-- All Screens, which have virtual workspace.
lookupScreens :: VirtualWorkspace -> WindowSet -> [ScreenId]
lookupScreens vw    = map unmarshallS . filter hasWorkspace . workspaceTags
  where
    hasWorkspace :: PhysicalWorkspace -> Bool
    hasWorkspace pw = unmarshallW pw == vw

-- All virtual workspaces, which Screen has.
screenWorkspaces :: ScreenId -> WindowSet -> [VirtualWorkspace]
screenWorkspaces sc = map unmarshallW . filter onScreen . workspaceTags
  where
    onScreen :: PhysicalWorkspace -> Bool
    onScreen pw     = unmarshallS pw == sc

-- Move out all windows from virtual workspace on specified Screen.
squashWorkspace' :: ScreenId -> VirtualWorkspace -> WindowSet -> WindowSet
squashWorkspace' sc vw s = shiftAll' to from s
  where
    from :: PhysicalWorkspace
    from            = marshall sc vw
    -- Move to workspace with matched virtual name on other Screen, if any, or
    -- to first workspace.
    to :: PhysicalWorkspace
    to              = case filter (/= sc) (lookupScreens vw s) of
      []        -> W.tag . head $ W.workspaces s
      (sc2 : _) -> marshall sc2 vw

-- Move out all windows from specified Screen.
squashScreen' :: ScreenId -> WindowSet -> WindowSet
squashScreen' sc    = flip (apList . map (squashWorkspace' sc))
                        <*> screenWorkspaces sc

-- On current screen.
squashWorkspace :: VirtualWorkspace -> WindowSet -> WindowSet
squashWorkspace vw  = currentScreen >>= flip squashWorkspace' vw

-- On last Screen. `rescreen` (from XMonad.Operations), which xmonad runs on
-- screen configuration change, always renumber screens. So when one monitor
-- had been unplugged, no matter which number it have - always the last Screen
-- gone.
squashScreen :: WindowSet -> WindowSet
squashScreen        = lastScreen >>= squashScreen'

{-

onScreenChange :: Event -> X All
onScreenChange (ConfigureEvent {ev_window = w}) = do
    whenX (isRoot w) $ do
      n <- countScreens
      if (n > 1)
        then do
          layoutScreens 2 (fixedLayout [Rectangle 0 0 640 480, Rectangle 641 481 1040 570])
        else rescreen
    return (All True)
onScreenChange   _  = return (All True)
-}



-- Event hook.
onScreenChange :: Event -> X All
onScreenChange (ConfigureEvent {ev_window = w}) = do
    whenX (isRoot w) $ do
      c <- countScreens
      withWindowSet $ \s -> do
        let n = allScreens s
        mapM_ (windows . squashScreen' . S) [c..n]
    return (All True)
onScreenChange   _  = return (All True)


{-
    in  if null xs 
                          ss = lookupScreens vws s
                          rs = filter n ss
                      in  map (shiftAll marshall n) vws

squashScreens :: Int -> WindowSet -> WindowSet
squashScreens availScrs ss
  | availScrs < usedScreens ss
                    = undefined
  | otherwise       = ss
  where
    usedScreens :: WindowSet -> Int
    usedScreens     = length . nub . map (unmarshallS . W.tag) . W.workspaces
-}

{-
-- Number of used screens according to workspace tags (assume
-- IndependentScreens).
usedScreens :: XConfig l -> Int
usedScreens         = length . nub . map unmarshallS . workspaces
-}

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
      , ((m .|. shiftMask , xK_u), windows $ squashScreen)

      -- Audio keys.
      , ((0,     xF86XK_AudioLowerVolume), spawn "amixer set Master 1311-")
      -- FIXME: This really not exactly what i want. I want, that if sound is
      -- muted, one VolUp only unmutes it. Otherwise, just VolUp-s.
      , ((0,     xF86XK_AudioRaiseVolume), spawn $ "amixer set Master unmute; "
          					 ++ "amixer set Master 1311+")
      , ((0,     xF86XK_AudioMute       ), spawn "amixer set Master mute")
      ]

{-
      -- Move all windows.
      ++ [(( m .|. shiftMask .|. controlMask, k)
           , windows $ shiftAll i)
                | (i, k) <- zip (map show [1..9]) [xK_1 .. xK_9]]
-}

      -- IndependentScreens .
      ++ [((m .|. ms, k), windows $ onCurrentScreen f i)
                | (i, k) <- zip (map show [1..9]) [xK_1 .. xK_9]
                         , (f, ms) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

      -- Squash Workspace.
      ++ [(( m .|. shiftMask .|. controlMask, k)
           , windows $ squashWorkspace i)
                | (i, k) <- zip (map show [1..9]) [xK_1 .. xK_9]]

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

