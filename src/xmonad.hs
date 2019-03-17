
import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.DebugWindow
import XMonad.Hooks.DebugStack
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops

import Data.List

main :: IO ()
main = do
        let xcf = def
                    { modMask = mod4Mask
                    , layoutHook = avoidStruts (layoutHook def)
                    -- Lower dock before unmanaging. The position of
                    -- `lowerDock` in `ManageHook` does not matter, because it
                    -- only runs X actions.
                    , manageHook = lowerDock <+> manageDocks <+> manageHook def
                    , handleEventHook = fullscreenEventHook <+> docksEventHook <+> handleEventHook def
                    , startupHook = docksStartupHook <+> startupHook def
                    }
                    `additionalKeys`
                    [ ((mod4Mask, xK_b), sendMessage ToggleStruts)
                    , ((mod4Mask, xK_x), spawn "xmobar ~/.xmobarrcAlt")
                    , ((mod4Mask, xK_m), spawn "chromium")
                    , ((mod4Mask, xK_r), refresh)
                    ]
        xmonad xcf

-- Change order of `debugStack` output to natural..
debugSt :: X ()
debugSt = debugStackString >>= trace . unlines . reverse . lines

logH :: X ()
logH    = do
    trace "Tree..."
    d <- asks display
    r <- asks theRoot
    (_, _, ts) <- liftIO $ queryTree d r
    ls <- mapM debugWindow ts
    trace (intercalate "\n" ls)

-- | Restack dock under lowest managed window.
lowerDock :: ManageHook
lowerDock = checkDock --> do
    w <- ask
    mlw <- liftX $ findLowest
    case mlw of
      Just lw   -> liftX $ do
        d <- asks display
        s <- debugWindow lw
        trace "_Before_ restacking dock:"
        logH
        trace $ "Lowest managed window: " ++ s
        liftIO $ restackWindows d [lw, w]
        trace "_After_ restack dock:"
        logH
        return idHook
      Nothing   -> return idHook

-- | Find lowest managed window.
findLowest :: X (Maybe Window)
findLowest  = withWindowSet $ \ws -> do
    d <- asks display
    r <- asks theRoot
    (_, _, ts) <- liftIO $ queryTree d r
    return (find (`W.member` ws) ts)

