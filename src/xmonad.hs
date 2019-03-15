
import XMonad
import XMonad.Operations
import XMonad.Util.EZConfig
import XMonad.Hooks.DebugStack
import qualified XMonad.StackSet as W

import Control.Exception
import Numeric
import Data.List

main :: IO ()
main = do
        let xcf = def
                    { modMask = mod4Mask
                    , logHook = debugStack >> logH >> logHook def
                    }
        xmonad xcf


logH :: X ()
logH    = withWindowSet $ \ws -> do
    let cs = maybe id (showWs . W.integrate) . W.stack . W.workspace . W.current $ ws
    trace (cs "")
    trace "Huy..."
    d <- asks display
    r <- asks theRoot
    (_, _, ts) <- liftIO $ queryTree d r
    trace (showWs ts "")
  where
    showWs :: [Window] -> ShowS
    showWs      = fmap (intercalate "\n") . mapM showHex
