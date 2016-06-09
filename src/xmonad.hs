{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Layout.LayoutScreens
import XMonad.Util.EZConfig (additionalKeys)

import Control.Monad

import Sgf.Control.Lens
import Sgf.XMonad.Config
import Sgf.XMonad.Restartable
import Sgf.XMonad.Restartable.Firefox
import Sgf.XMonad.Restartable.XTerm
import Sgf.XMonad.VNC
import Sgf.XMonad.Util.EntryHelper
import Sgf.XMonad.Trace


main :: IO ()
main                = withHelper $ do
    -- FIXME: Spawn process directly, not through shell.
    let scf = def {programs = programs def ++ myPrograms}
        xcf = session scf
                . (additionalKeys <*> myKeys)
                $ (fromSgfXConfig def)
                    {
                    -- Do not set terminal here: edit `xterm` value instead.
                    -- Because proper conversion from Program to String (and
                    -- back) should be done with respect to shell escaping
                    -- rules, it's simpler to just redefine 'mod+shift+enter'
                    -- to use Program value (`xterm`). I set terminal here,
                    -- though, to make it roughly match to `xterm` value and
                    -- to avoid conversion issues i just throw away all
                    -- arguments: at least it's safe..
                    terminal = viewA progBin xterm
                    --, logHook = traceWindowSet
                    }
    handleVnc xcf >>= xmonad

myPrograms :: [ProgConfig l]
myPrograms          = [ addProg xtermUser, addProg xtermRoot
                      , addProg firefox
                      , addProg skype
                      , addProg pidgin
                      , addProg xclock
                      ]

-- Session with instant messengers.
sessionIMKey :: (ButtonMask, KeySym)
sessionIMKey        = (shiftMask, xK_s)
-- Minimal session key and all other session keys. Should be used for
-- programs, which must run in any session (e.g. firefox or terminal).
sessionKeys :: [(ButtonMask, KeySym)]
sessionKeys         = [(0, xK_s), sessionIMKey]

-- Programs {{{

-- With such definition, `xterm == xtermUser`, but that should not matter,
-- because 'xterm' launched untracked using `runP` (and, thus,  should not
-- appear in Extensible State).
xterm :: XTerm
xterm               = defaultXTerm
xtermUser :: XTerm
xtermUser           = setA progWorkspace "2"
                        . setA progLaunchKey ((0, xK_x) : sessionKeys)
                        $ defaultXTerm
-- In fact, title "root" will be immediately overwritten by shell. But for
-- xmonad values 'xtermUser' and 'xtermRoot' will no longer equal.
xtermRoot :: XTerm
xtermRoot           = setA progWorkspace "3"
                        . setA (progArgs . xtermTitle) "root"
                        . setA progLaunchKey ((shiftMask, xK_x) : sessionKeys)
                        $ defaultXTerm

firefox :: Firefox
firefox             = setA progStartup False
                        . setA progWorkspace "1"
                        . setA progLaunchKey ((0, xK_f) : sessionKeys)
                        . setA (progArgs . firefoxProfile) "sgf"
                        $ defaultFirefox

skype :: Program NoArgs
skype               = setA progBin "skype"
                        . setA progStartup False
                        . setA progWorkspace "4"
                        . setA progLaunchKey [(0, xK_i), sessionIMKey]
                        $ defaultProgram

pidgin :: Program NoArgs
pidgin              = setA progBin "pidgin"
                        . setA progStartup False
                        . setA progWorkspace "4"
                        . setA progLaunchKey [(shiftMask, xK_i) , sessionIMKey]
                        $ defaultProgram

xclock :: Program NoArgs
xclock              = setA progBin "xclock"
                        . setA progStartup False
                        . setA progWorkspace "7"
                        . setA progLaunchKey [(shiftMask, xK_d), sessionIMKey]
                        $ defaultProgram
-- END programs }}}

-- Some debug traces.

-- Log programs stored in Extensible State.
tracePrograms :: X ()
tracePrograms       = do
    trace "Tracked programs:"
    --traceP xmobar
    --traceP feh
    --traceP firefox
{-
    traceP xtermUser
    traceP xtermRoot
    traceP xmobar
    traceP trayer
-}

-- Key for hiding all docks defined by handleDocks, keys for hiding particular
-- dock, if any, defined in that dock definition (see above).
myKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
myKeys XConfig {modMask = m} =
      [ 
      --((m .|. shiftMask, xK_p), spawn "exec gmrun")
      ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0,           xK_Print), spawn "scrot")
      -- For testing two screens.
      --, ((m .|. shiftMask,                 xK_space), layoutScreens 2 testTwoScreen)
      --, ((m .|. controlMask .|. shiftMask, xK_space), rescreen)

      -- Programs.
      -- Use Program `xterm` to determine which terminal to launch instead of
      -- XConfig 'terminal' record.
      , ( (m .|. shiftMask, xK_Return), void (runP xterm))
      , ( (m .|. shiftMask, xK_f)
        , spawn "exec firefox --new-instance -ProfileManager")
      ]

-- Two screens dimensions for layoutScreen. Two xmobars have height 17, total
-- resolution is 1680x1050 .
testTwoScreen :: FixedLayout a
testTwoScreen       = fixedLayout
                        [ Rectangle 0 17 1680 536
                        , Rectangle 0 553 1680 480
                        ]


