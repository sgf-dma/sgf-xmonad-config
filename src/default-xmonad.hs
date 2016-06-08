{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Util.EZConfig (additionalKeys)

import Control.Monad
import System.Info
import System.FilePath

import Sgf.Control.Lens
import Sgf.XMonad.Config
import Sgf.XMonad.Restartable
import Sgf.XMonad.Restartable.XTerm
import Sgf.XMonad.VNC
import Sgf.XMonad.Util.EntryHelper


main :: IO ()
main                = withHelper $ do
    dir <- getXMonadDir
    let binn = "xmonad-"++arch++"-"++os
        bin  = dir </> binn
        errMsg  = "This ie the default xmonad config. "
                    ++ "It may be loaded, because user's xmonad binary " ++ bin
                    ++ " was not found or failed to start."
    -- FIXME: Spawn process directly, not through shell.
    let xcf = session def
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
                    , startupHook = do
                        trace errMsg
                        spawn ("xmessage \"" ++ errMsg ++ "\"")
                    }
    handleVnc xcf >>= xmonad

-- With such definition, `xterm == xtermUser`, but that should not matter,
-- because 'xterm' launched untracked using `runP` (and, thus,  should not
-- appear in Extensible State).
xterm :: XTerm
xterm               = defaultXTerm

-- Key for hiding all docks defined by handleDocks, keys for hiding particular
-- dock, if any, defined in that dock definition (see above).
myKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
myKeys XConfig {modMask = m} =
      [ 
      -- Use Program `xterm` to determine which terminal to launch instead of
      -- XConfig 'terminal' record.
        ((m .|. shiftMask, xK_Return), void (runP xterm))
      ]

