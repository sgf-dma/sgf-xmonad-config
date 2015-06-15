{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import XMonad
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog (shorten, xmobarColor)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.LayoutScreens
import XMonad.Util.EZConfig (additionalKeys)

import Data.Maybe
import Control.Monad
import Graphics.X11.ExtraTypes.XF86 -- For media keys.
import qualified Data.Map as M
import Data.Monoid
import Control.Applicative
import System.Process

import System.FilePath
import System.Directory
import Codec.Binary.UTF8.String (encodeString) 

import Sgf.Control.Lens
import Sgf.XMonad.Restartable
import Sgf.XMonad.Docks
import Sgf.XMonad.Docks.Xmobar
import Sgf.XMonad.VNC
import Sgf.XMonad.Util.EntryHelper

main :: IO ()
main                = withHelper main_0

main_0 :: IO ()
main_0              = do
    -- FIXME: Spawn process directly, not through shell.
    let xcf = handleFullscreen
                . handleDocks (Just (0, xK_b))
                . handleProgs (Just (0, xK_s)) (myPrograms ++ myDocks)
                . (additionalKeys <*> myKeys)
                $ defaultConfig
                    {
                    -- Workspace "lock" is for xtrlock only and it is
                    -- inaccessible for workspace switch keys.
                    workspaces = map show [1..9] ++ ["lock"]
                    , modMask = mod4Mask
                    , focusFollowsMouse = False
                    , terminal = "xterm -fg black -bg white"
                    --, manageHook = traceFloat
                    --, logHook = traceWindowSet
                    }
    handleVnc xcf >>= xmonad

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

myDocks :: LayoutClass l Window => [ProgConfig l]
myDocks     = addDock trayer : map addDock [xmobar, xmobarAlt]

myPrograms :: LayoutClass l Window =>[ProgConfig l]
myPrograms          = [ addProg feh
                      , addProg xtermUser, addProg xtermRoot
                      , addProg firefox
                      , addProg skype
                      , addProg pidgin
                      ]

-- Session with instant messengers.
sessionIMKey :: (ButtonMask, KeySym)
sessionIMKey        = (shiftMask, xK_s)
-- Minimal session key and all other session keys. Should be used for
-- programs, which must run in any session (e.g. firefox or terminal).
sessionKeys :: [(ButtonMask, KeySym)]
sessionKeys         = [(0, xK_s), sessionIMKey]

-- Docks {{{

-- Note, that because i redefine PP, Xmobar implementation assumes, that
-- StdinReader is used in .xmobarrc, and opens pipe to xmobar. Thus, if
-- StdinReader does not actually used in template in .xmobarrc, xmonad will
-- freeze, when pipe fills up. See
-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions#XMonad_is_frozen.21
-- .
-- Main xmobar, which does not have hiding (Strut toggle) key.
xmobar :: Xmobar
xmobar              = setA xmobarPP (Just (setA ppTitleL t defaultXmobarPP))
                        $ defaultXmobar
  where
    t :: String -> String
    t               = xmobarColor "green" "" . shorten 50
-- Alternative xmobar, which has hiding (Strut toggle) key.
xmobarAlt :: Xmobar
xmobarAlt           = setA xmobarConf ".xmobarrcAlt"
                        . setA xmobarToggle (Just (shiftMask, xK_b))
                        $ defaultXmobar

newtype Trayer      = Trayer Program
  deriving (Eq, Show, Read, Typeable)
instance Monoid Trayer where
    (Trayer x) `mappend` (Trayer y) = Trayer (x `mappend` y)
    mempty          = Trayer mempty
instance ProcessClass Trayer where
    pidL f (Trayer x)   = Trayer <$> pidL f x
instance RestartClass Trayer where
    runP (Trayer x)     = Trayer <$> runP x
    doLaunchP           = restartP
instance DockClass Trayer where

trayer :: Trayer
trayer              = Trayer $ setA progBin "trayer"
                        . setA progArgs
                            [ "--edge", "top", "--align", "right"
                            , "--SetDockType", "true"
                            , "--SetPartialStrut", "true"
                            , "--expand", "true", "--width", "10"
                            , "--transparent", "true" , "--tint", "0x191970"
                            , "--height", "12"
                            ]
                        $ defaultProgram

-- END Docks }}}
-- Programs {{{

-- Use `xsetroot -grey`, if no .fehbg found.
newtype Feh         = Feh Program
  deriving (Eq, Show, Read, Typeable)
instance Monoid Feh where
    (Feh x) `mappend` (Feh y) = Feh (x `mappend` y)
    mempty          = Feh mempty
instance ProcessClass Feh where
    pidL f (Feh x)  = Feh <$> pidL f x
instance RestartClass Feh where
    runP (Feh x)    = do
        cmd <- liftIO $ do
          h <- getHomeDirectory
          let f = h </> ".fehbg"
          b <- doesFileExist f
          if b
            then readFile f
            else return "xsetroot -grey"
        Feh <$> runP (setA progArgs ["-c", encodeString cmd] x)
feh :: Feh
feh                 = Feh   $ setA progBin "/bin/sh"
                            . setA progArgs ["-c", ""]
                            $ defaultProgram

-- User terminal.
newtype XTermUser   = XTermUser Program
  deriving (Eq, Show, Read, Typeable)
instance Monoid XTermUser where
    (XTermUser x) `mappend` (XTermUser y) = XTermUser (x `mappend` y)
    mempty          = XTermUser mempty
instance ProcessClass XTermUser where
    pidL f (XTermUser x)    = XTermUser <$> pidL f x
instance RestartClass XTermUser where
    runP (XTermUser x)      = XTermUser <$> runP x
    manageP (XTermUser _)   = doShift "2"
    launchKey               = const ((0, xK_x) : sessionKeys)
xtermUser :: XTermUser
xtermUser           = XTermUser
                        . setA progBin "xterm"
                        . setA progArgs ["-fg", "black", "-bg", "white"
                                        , "-e", "tmux at -t main"]
                        $ defaultProgram

-- Root terminal.
newtype XTermRoot   = XTermRoot Program
  deriving (Eq, Show, Read, Typeable)
instance Monoid XTermRoot where
    (XTermRoot x) `mappend` (XTermRoot y) = XTermRoot (x `mappend` y)
    mempty          = XTermRoot mempty
instance ProcessClass XTermRoot where
    pidL f (XTermRoot x)    = XTermRoot <$> pidL f x
instance RestartClass XTermRoot where
    runP (XTermRoot x)      = XTermRoot <$> runP x
    manageP (XTermRoot _)   = doShift "3"
    launchKey               = const ((0, xK_x) : sessionKeys)
xtermRoot :: XTermRoot
xtermRoot           = XTermRoot
                        . setA progBin "xterm"
                        . setA progArgs ["-fg", "black", "-bg", "white"
                                        , "-e", "tmux at -t root"]
                        $ defaultProgram

newtype Firefox     = Firefox Program
  deriving (Eq, Show, Read, Typeable)
instance Monoid Firefox where
    (Firefox x) `mappend` (Firefox y) = Firefox (x `mappend` y)
    mempty          = Firefox mempty
instance ProcessClass Firefox where
    pidL f (Firefox x)  = Firefox <$> pidL f x
instance RestartClass Firefox where
    runP (Firefox x)    = Firefox <$> runP x
    manageP (Firefox _) = doShift "1"
    launchAtStartup     = const False
    launchKey           = const ((0, xK_f) : sessionKeys)
firefox :: Firefox
firefox             = Firefox
                        . setA progBin "firefox"
                        $ defaultProgram

newtype Skype       = Skype Program
  deriving (Eq, Show, Read, Typeable)
instance Monoid Skype where
    (Skype x) `mappend` (Skype y) = Skype (x `mappend` y)
    mempty          = Skype mempty
instance ProcessClass Skype where
    pidL f (Skype x)    = Skype <$> pidL f x
instance RestartClass Skype where
    runP (Skype x)      = Skype <$> runP x
    manageP (Skype _)   = doShift "4"
    launchAtStartup     = const False
    launchKey           = const [(0, xK_i), sessionIMKey]
skype :: Skype
skype               = Skype $ setA progBin "skype" defaultProgram

newtype Pidgin      = Pidgin Program
  deriving (Eq, Show, Read, Typeable)
instance Monoid Pidgin where
    (Pidgin x) `mappend` (Pidgin y) = Pidgin (x `mappend` y)
    mempty          = Pidgin mempty
instance ProcessClass Pidgin where
    pidL f (Pidgin x)   = Pidgin <$> pidL f x
instance RestartClass Pidgin where
    runP (Pidgin x)     = Pidgin <$> runP x
    manageP (Pidgin _)  = doShift "4"
    launchAtStartup     = const False
    launchKey           = const [(shiftMask, xK_i), sessionIMKey]
pidgin :: Pidgin
pidgin              = Pidgin $ setA progBin "pidgin" defaultProgram

-- Just for testing.
newtype XClock      = XClock Program
  deriving (Eq, Show, Read, Typeable)
instance Monoid XClock where
    (XClock x) `mappend` (XClock y) = XClock (x `mappend` y)
    mempty          = XClock mempty
instance ProcessClass XClock where
    pidL f (XClock x)   = XClock <$> pidL f x
instance RestartClass XClock where
    runP (XClock x)     = XClock <$> runP x
    manageP (XClock _)  = doShift "7"
xclock :: XClock
xclock          = XClock $ setA progBin "xclock" defaultProgram

-- END programs }}}

-- Some debug traces.

-- Log all windows (tiled and floating).
traceWindowSet :: X ()
traceWindowSet      = do
    trace "Windows:"
    withWindowSet $ \ws -> do
      whenJust (W.stack . W.workspace . W.current $ ws) $ \s -> do
        ts <- mapM (runQuery title) (W.integrate s)
        trace $ "Tiled: " ++ show ts
      fs <- mapM (runQuery title) (M.keys . W.floating $ ws)
      when (not (null fs)) $ trace ("Floating: " ++ show fs)

-- Log programs stored in Extensible State.
tracePrograms :: X ()
tracePrograms       = do
    trace "Tracked programs:"
    traceP xtermUser
    traceP xtermRoot
    traceP xmobar
    traceP trayer
    traceP feh

-- Log windows, which would be made floating by default (particularly, by
-- `manage` from XMonad/Operations.hs), and why they would.
traceFloat :: ManageHook
traceFloat          = do
    w <- ask
    t <- title
    liftX $ withDisplay $ \d -> do
      sh <- io $ getWMNormalHints d w
      let isFixedSize = sh_min_size sh /= Nothing
                          && sh_min_size sh == sh_max_size sh
      isTransient <- isJust <$> io (getTransientForHint d w)
      when isFixedSize $
        trace ("Fixed size window " ++ show w ++ " \"" ++ t ++ "\"")
      when isTransient $
        trace ("Transient window " ++ show w ++ " \"" ++ t ++ "\"")
    return idHook

-- Key for hiding all docks defined by handleDocks, keys for hiding particular
-- dock, if any, defined in that dock definition (see above).
myKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
myKeys XConfig {modMask = m} =
      [ 
      --((m .|. shiftMask, xK_p), spawn "exec gmrun")
        ((m .|. shiftMask, xK_z), lock)
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0,           xK_Print), spawn "scrot")
      -- For testing two screens.
      --, ((m .|. shiftMask,                 xK_space), layoutScreens 2 testTwoScreen)
      --, ((m .|. controlMask .|. shiftMask, xK_space), rescreen)

      -- Programs.
      , ( (m .|. shiftMask, xK_f)
        , spawn "exec firefox -no-remote -ProfileManager")
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

