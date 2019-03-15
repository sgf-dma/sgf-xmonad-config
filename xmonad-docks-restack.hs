
import XMonad
import XMonad.Operations
import XMonad.Util.EZConfig
import XMonad.Hooks.DebugStack
import qualified XMonad.StackSet as W

import Control.Exception

main :: IO ()
main = do
        let xcf = def
                    { modMask = mod4Mask
                    , logHook = debugStack >> logH >> logHook def
                    }
        xmonad xcf


logH :: X ()
logH    = withWindowSet $ \ws -> do
    let cs = maybe [] (map show . W.integrate) . W.stack $ ws
    trace (concat cs)

