
import XMonad
import XMonad.Config.Xfce
import XMonad.Layout.NoBorders

import Graphics.X11.ExtraTypes.XF86 -- For media keys.
import qualified Data.Map as M
import Control.Applicative

main                = xmonad
                      . alterKeys myKeys
                      $ xfceConfig
                            { modMask = mod4Mask
                            , focusFollowsMouse = False
                            , layoutHook = smartBorders $ layoutHook xfceConfig
                            }


-- Replace matched key bindings in defKEys using ks 's ones. Add new ones from
-- ks. (below ((->) r) functor is used).
alterKeys ks conf@(XConfig {keys = defKeys}) =
    conf {keys = foldr (uncurry M.insert) <$> defKeys <*> ks}

-- My key bindings. They are intended for use with alterKeys only.
myKeys cl =
    [ ((modMask cl .|. shiftMask, xK_p), spawn "exec gmrun")
    , ((0,     xF86XK_AudioLowerVolume), spawn "amixer set Master 1311-")
    -- FIXME: This really not exactly what i want. I want, that if sound is
    -- muted, one VolUp only unmutes it. Otherwise, just VolUp-s.
    , ((0,     xF86XK_AudioRaiseVolume), spawn $ "amixer set Master unmute; "
						 ++ "amixer set Master 1311+")
    , ((0,     xF86XK_AudioMute       ), spawn "amixer set Master toggle")
    ]

