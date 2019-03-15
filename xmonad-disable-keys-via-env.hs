
import XMonad
import XMonad.Operations
import XMonad.Util.EZConfig

import System.Environment
import Control.Exception

main :: IO ()
main = do
        let xcf = def
                    { modMask = mod4Mask
                    , handleExtraArgs = disableKeysE
                    }
                    `additionalKeys`
                    [ ((mod4Mask, xK_d), disableKeysOn) ]
        xmonad xcf

disableKeysOn :: X ()
disableKeysOn       = do
    trace "Preparing to disable keys and restarting.."
    io $ setEnv "XMONAD_DISABLE_KEYS" "1"
    restart "xmonad" True

disableKeysE :: [String] -> XConfig Layout -> IO (XConfig Layout)
disableKeysE _ xcf = do
    me <- lookupEnv "XMONAD_DISABLE_KEYS"
    case me of
      Just _    -> do
        trace "Disabling all keys."
        unsetEnv "XMONAD_DISABLE_KEYS"
        return (xcf {keys = \_ -> mempty})
      Nothing   -> return xcf

