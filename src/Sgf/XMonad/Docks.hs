{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Docks
    ( DockConfig
    , addDock
    , handleDocks
    , DockClass (..)
    , ppCurrentL
    , ppVisibleL
    , ppHiddenL
    , ppHiddenNoWindowsL
    , ppUrgentL
    , ppSepL
    , ppWsSepL
    , ppTitleL
    , ppLayoutL
    , ppOrderL
    , ppSortL
    , ppExtrasL
    , ppOutputL
    )
  where

import Data.Maybe
import Data.List
import Data.Monoid
import Control.Monad.State
import Control.Applicative
import System.Posix.Types (ProcessID)

import XMonad
import XMonad.Hooks.ManageDocks hiding (docksEventHook)
import XMonad.Hooks.DynamicLog
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.XUtils (fi)
import Foreign.C.Types (CLong)

import Sgf.Control.Lens
import Sgf.XMonad.Restartable


-- Store some records of XConfig modified for particular dock.
data DockConfig l   = DockConfig
                        { dockLogHook :: X ()
                        , dockStartupHook :: X ()
                        , dockKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
                        }

-- Create DockConfig from DockClass instance.
addDock :: (RestartClass a, DockClass a, LayoutClass l Window) =>
               a -> DockConfig l
addDock d           = DockConfig
      -- Log to dock according to its PP .
      { dockLogHook     = dockLog d
      -- Restart dock at xmonad startup. reinitPP should be done before
      -- restartP, because i may check or fill some PP values in dock's
      -- RestartClass instance (runP, particularly).
      , dockStartupHook = liftA2 (<*) reinitPP restartP d
      -- Key for toggling Struts of this Dock.
      , dockKeys        = toggleDock d
      }

-- Merge DockConfig-s into existing XConfig properly. Also takes a key for
-- toggling visibility (Struts) of all docks.
handleDocks :: LayoutClass l Window => (ButtonMask, KeySym)
               -> [DockConfig (ModifiedLayout AvoidStruts l)]
               -> XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
handleDocks t ds cf = addDockKeys $ cf
      -- First, de-manage dock applications.
      { manageHook = manageDocks <+> manageHook cf
      -- Then refresh screens after new dock appears.
      , handleEventHook = docksEventHook <+> handleEventHook cf
      -- Reduce Rectangle available for other windows according to Struts.
      , layoutHook = avoidStruts (layoutHook cf)
      -- Log to all docks according to their PP .
      , logHook = mapM_ dockLogHook ds >> logHook cf
      -- Restart all docks at xmonad startup.
      , startupHook = mapM_ dockStartupHook ds >> startupHook cf
      }
  where
    -- Join keys for toggling Struts of all docks and of each dock, if
    -- defined.
    --addDockKeys :: XConfig l1 -> XConfig l1
    addDockKeys     = additionalKeys <*> (concat <$> sequence
                        (toggleAllDocks t : map dockKeys ds))


class ProcessClass a => DockClass a where
    dockToggleKey   :: a -> Maybe (ButtonMask, KeySym)
    ppL             :: LensA a (Maybe PP)

toggleAllDocks :: (ButtonMask, KeySym) -> XConfig l
               -> [((ButtonMask, KeySym), X ())]
toggleAllDocks (mk, k) XConfig {modMask = m} =
                        [((m .|. mk, k), sendMessage ToggleStruts)]

toggleDock :: DockClass a => a -> XConfig l -> [((ButtonMask, KeySym), X ())]
toggleDock x (XConfig {modMask = m}) = maybeToList $ do
    (mk, k) <- dockToggleKey x
    return ((m .|. mk, k), toggleProcessStruts x)

-- Toggle struts for any ProcessClass instance.
toggleProcessStruts :: ProcessClass a => a -> X ()
toggleProcessStruts = withProcess $ \x -> do
    maybe (return ()) togglePidStruts (viewA pidL x)
    return x
  where
    -- Toggle all struts, which specified PID have.
    togglePidStruts :: ProcessID -> X ()
    togglePidStruts pid = withDisplay $ \dpy -> do
        rootw <- asks theRoot
        (_, _, wins) <- io $ queryTree dpy rootw
        ws <- filterM winPid wins
        ss <- mapM getStrut ws
        let ds = nub . map (\(s, _, _, _) -> s) . concat $ ss
        mapM_ (sendMessage . ToggleStrut) ds
      where
        winPid :: Window -> X Bool
        winPid w        = do
            mp <- getProp32s "_NET_WM_PID" w
            return $ case mp of
              Just [pid'] -> pid == fi pid'
              _           -> False

-- Copy from XMonad.Hooks.ManageDocks .
type Strut = (Direction2D, CLong, CLong, CLong)

-- | Gets the STRUT config, if present, in xmonad gap order
getStrut :: Window -> X [Strut]
getStrut w = do
    msp <- getProp32s "_NET_WM_STRUT_PARTIAL" w
    case msp of
        Just sp -> return $ parseStrutPartial sp
        Nothing -> fmap (maybe [] parseStrut) $ getProp32s "_NET_WM_STRUT" w
 where
    parseStrut xs@[_, _, _, _] = parseStrutPartial . take 12 $ xs ++ cycle [minBound, maxBound]
    parseStrut _ = []

    parseStrutPartial [l, r, t, b, ly1, ly2, ry1, ry2, tx1, tx2, bx1, bx2]
     = filter (\(_, n, _, _) -> n /= 0)
        [(L, l, ly1, ly2), (R, r, ry1, ry2), (U, t, tx1, tx2), (D, b, bx1, bx2)]
    parseStrutPartial _ = []
-- End copy from XMonad.Hooks.ManageDocks .

-- docksEventHook version from xmobar tutorial (5.3.1 "Example for using the
-- DBus IPC interface with XMonad"), which refreshes screen on unmap events as
-- well.
docksEventHook :: Event -> X All
docksEventHook e = do
    when (et == mapNotify || et == unmapNotify) $
        whenX ((not `fmap` (isClient w)) <&&> runQuery checkDock w) refresh
    return (All True)
    where w  = ev_window e
          et = ev_event_type e

dockLog :: DockClass a => a ->  X ()
dockLog             = withProcess $ \x -> do
    maybe (return ()) dynamicLogWithPP (viewA ppL x)
    return x

-- Because i can't save PP values in persistent Extensible State (there is
-- neither Show nor Read instance for PP), i need to reinitialize them each
-- time at the start (in startupHook).
reinitPP :: DockClass a => a -> X ()
reinitPP y          = withProcess (\x -> return (setA ppL (viewA ppL y) x)) y


-- Lenses to PP.
ppCurrentL :: LensA PP (WorkspaceId -> String)
ppCurrentL f z@(PP {ppCurrent = x})
                    = fmap (\x' -> z{ppCurrent = x'}) (f x)
ppVisibleL :: LensA PP (WorkspaceId -> String)
ppVisibleL f z@(PP {ppVisible = x})
                    = fmap (\x' -> z{ppVisible = x'}) (f x)
ppHiddenL :: LensA PP (WorkspaceId -> String)
ppHiddenL f z@(PP {ppHidden = x})
                    = fmap (\x' -> z{ppHidden = x'}) (f x)
ppHiddenNoWindowsL :: LensA PP (WorkspaceId -> String)
ppHiddenNoWindowsL f z@(PP {ppHiddenNoWindows = x})
                    = fmap (\x' -> z{ppHiddenNoWindows = x'}) (f x)
ppUrgentL :: LensA PP (WorkspaceId -> String)
ppUrgentL f z@(PP {ppUrgent = x})
                    = fmap (\x' -> z{ppUrgent = x'}) (f x)
ppSepL :: LensA PP (String)
ppSepL f z@(PP {ppSep = x})
                    = fmap (\x' -> z{ppSep = x'}) (f x)
ppWsSepL :: LensA PP (String)
ppWsSepL f z@(PP {ppWsSep = x})
                    = fmap (\x' -> z{ppWsSep = x'}) (f x)
ppTitleL :: LensA PP (String -> String)
ppTitleL f z@(PP {ppTitle = x})
                    = fmap (\x' -> z{ppTitle = x'}) (f x)
ppLayoutL :: LensA PP (String -> String)
ppLayoutL f z@(PP {ppLayout = x})
                    = fmap (\x' -> z{ppLayout = x'}) (f x)
ppOrderL :: LensA PP ([String] -> [String])
ppOrderL f z@(PP {ppOrder = x})
                    = fmap (\x' -> z{ppOrder = x'}) (f x)
ppSortL :: LensA PP (X ([WindowSpace] -> [WindowSpace]))
ppSortL f z@(PP {ppSort = x})
                    = fmap (\x' -> z{ppSort = x'}) (f x)
ppExtrasL :: LensA PP ([X (Maybe String)])
ppExtrasL f z@(PP {ppExtras = x})
                    = fmap (\x' -> z{ppExtras = x'}) (f x)
ppOutputL :: LensA PP (String -> IO ())
ppOutputL f z@(PP {ppOutput = x})
                    = fmap (\x' -> z{ppOutput = x'}) (f x)

