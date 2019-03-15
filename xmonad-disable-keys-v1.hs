
import XMonad
import XMonad.Hooks.EwmhDesktops

import System.Directory
import System.FilePath


main :: IO ()
main = do
        let xcf = ewmh $ def
                    { modMask = mod4Mask
                    , handleExtraArgs = disableKeys
                    }
        xmonad xcf

disableKeys :: [String] -> XConfig Layout -> IO (XConfig Layout)
disableKeys _ xcf = do
    xd <- getXMonadDir
    let disableFn = xd </> "disable_keys"
    b <- doesFileExist disableFn
    if b
      then do
        trace "Hhuy"
        removeFile disableFn
        return (xcf {keys = \_ -> mempty})
      else return xcf

