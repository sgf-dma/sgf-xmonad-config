
import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.DebugWindow
import XMonad.Hooks.DebugStack
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.WindowProperties (getProp32s)

import Data.List
import Data.Monoid

main :: IO ()
main = do
        let xcf = def
                    { modMask = mod4Mask
                    , layoutHook = avoidStruts (layoutHook def)
                    , manageHook = addDock <+> manageDocks <+> manageHook def
                    , handleEventHook = delDock <+> fullscreenEventHook <+> docksEventHook <+> handleEventHook def
                    , startupHook = docksStartupHook <+> startupHook def
                    , logHook = logDL >> logHook def
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

data DockT  = DockA Window
            | DesktopA Window
  deriving (Show, Eq)

checkDesktop :: Query Bool
checkDesktop = ask >>= \w -> liftX $ do
    desk <- getAtom "_NET_WM_WINDOW_TYPE_DESKTOP"
    mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
    case mbr of
        Just rs -> return $ any (== desk) (map fromIntegral rs)
        _       -> return False

newtype DockList    = DockList [DockT]
  deriving (Show)

findDock :: Window -> [DockT] -> Maybe DockT
findDock w    = find go
  where
    go :: DockT -> Bool
    go (DockA x)    = w == x
    go (DesktopA x) = w == x

getPanels :: DockList -> [DockT]
getPanels (DockList ds) = filter go ds
  where
    go (DockA _)    = True
    go _            = False

getDesktops :: DockList -> [DockT]
getDesktops (DockList ds) = filter go ds
  where
    go (DesktopA _) = True
    go _            = False

unDock :: DockT -> Window
unDock (DockA x)    = x
unDock (DesktopA x) = x

instance ExtensionClass DockList where
    initialValue = DockList []


addDesktop :: MaybeManageHook
addDesktop    = checkDesktop  -?> do
    w <- ask
    liftX $ XS.modify (\(DockList zs) -> DockList (DesktopA w : zs))
    return idHook

addPanel :: MaybeManageHook
addPanel    = checkDock  -?> do
    w <- ask
    liftX $ XS.modify (\(DockList zs) -> DockList (DockA w : zs))
    return idHook

addDock :: ManageHook
addDock   = checkDock --> do
    composeOne [addDesktop, addPanel]
    liftX $ do
      dl <- XS.get
      let ds = getDesktops dl
          ps = getPanels dl
          rs = map unDock (ps ++ ds)
      d <- asks display
      trace "_Before_ lowering dock:"
      logH
      liftIO $ do
        lowerWindow d (head rs)
        restackWindows d rs
      trace "_After_ lowering dock:"
      logH
      return mempty

delDock :: Event -> X All
delDock (DestroyWindowEvent {ev_window = w}) = do
    DockList ds <- XS.get
    let md = w `findDock` ds
    case md of
      Just d    -> XS.put (DockList (d `delete` ds)) >> return mempty
      Nothing   -> return mempty
delDock _ = return mempty

logDL :: X ()
logDL   = do
    DockList ds <- XS.get
    trace $ "Docks list: " ++ intercalate ", " (map show ds)

