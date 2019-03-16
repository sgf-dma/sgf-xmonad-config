
import XMonad
import XMonad.Operations
import XMonad.Util.EZConfig
import XMonad.Util.DebugWindow
import XMonad.Hooks.DebugStack
import qualified XMonad.StackSet as W

import Control.Monad
import Control.Exception
import Numeric
import Data.List

main :: IO ()
main = do
        let xcf = def
                    { modMask = mod4Mask
                    , logHook = debugSt >> logH >> logHook def
                    }
        xmonad xcf

-- Change order of `debugStack` output to natural..
debugSt :: X ()
debugSt = debugStackString >>= trace . unlines . reverse . lines

logH :: X ()
logH    = withWindowSet $ \ws -> do
    trace "Huy..."
    d <- asks display
    r <- asks theRoot
    (_, _, ts) <- liftIO $ queryTree d r
    ls <- mapM debugWindow ts
    trace (intercalate "\n" ls)
  where
    showWs :: [Window] -> ShowS
    showWs      = fmap (intercalate "\n") . mapM showHex

