{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Docks
    ( handleDocks
    , DockClass (..)
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

-- Take list of keys for toggling visibility (Struts, in particular) for all
-- docks and list of docks to start. Each dock may have its own (Strut)
-- toggle.
handleDocks :: (RestartClass a, DockClass a, LayoutClass l Window) =>
               [(ButtonMask, KeySym)]   -- Toggle docks key(s).
               -> [a]                   -- Docks to start.
               -> XConfig l
               -> XConfig (ModifiedLayout AvoidStruts l)
handleDocks ts ds cf = 
    additionalKeys <*> dockKeys $ cf
      -- First, de-manage dock applications.
      { manageHook = manageDocks <+> manageHook cf
      -- Then refresh screens after new dock appears.
      , handleEventHook = docksEventHook <+> handleEventHook cf
      -- Reduce Rectangle available for other windows according to Struts.
      , layoutHook = avoidStruts (layoutHook cf)
      -- Log to all docks according to their PP .
      , logHook = mapM_ dockLog ds >> logHook cf
      -- Restart all docks at xmonad startup. reinitPP should be done before
      -- restartP, because i may check or fill some PP values in dock's
      -- RestartClass instance (runP, particularly).
      , startupHook = mapM_ (liftA2 (<*) reinitPP restartP) ds >>
                        startupHook cf
      }
  where
    -- Add keys for toggling Struts for all docks and for each dock, if
    -- defined.
    dockKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
    dockKeys        = fmap concat . sequence $
                        map toggleAllDocks ts ++ map toggleDock ds

class ProcessClass a => DockClass a where
    dockToggleKey   :: a -> Maybe (ButtonMask, KeySym)
    ppL             :: Lens a (Maybe PP)

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
    maybe (return ()) togglePidStruts (view pidL x)
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
    maybe (return ()) dynamicLogWithPP (view ppL x)
    return x

-- Because i can't save PP values in persistent Extensible State (there is
-- neither Show nor Read instance for PP), i need to reinitialize them each
-- time at the start (in startupHook).
reinitPP :: DockClass a => a -> X ()
reinitPP y          = withProcess (\x -> return (set ppL (view ppL y) x)) y

