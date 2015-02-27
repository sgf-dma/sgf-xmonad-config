{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.XMonad.Docks
    ( handleDocks
    , XmobarPID (..)
    , TrayerPID (..)
    , xmobarPP'
    )
  where

import Data.Maybe
import Data.List
import Data.Monoid
import Control.Monad.State
import Control.Applicative
import System.IO
import System.Posix.Types (ProcessID)

import XMonad
import XMonad.Hooks.ManageDocks hiding (docksEventHook)
import XMonad.Hooks.DynamicLog
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.XUtils (fi)
import Foreign.C.Types (CLong)

import Sgf.Data.List
import Sgf.XMonad.Util.Run (spawnPipe')
import Sgf.XMonad.Restartable

-- Take list of keys for toggling all docks and list of xmobar instances,
-- each may have each own toggle.
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
      -- Reduce Rectangle available for other windows.
      , layoutHook = avoidStruts (layoutHook cf)
      -- I can union keys explicitly
      --, keys = sequence [toggleBotDock, toggleDocks, keys x] >>=
      --    return . foldr M.union M.empty
      -- or use additionalKeys above.

      -- Log to all docks according to their PP .
      , logHook = mapM_ dockLog ds >> logHook cf
      --, startupHook = mapM_ restartP ds >> mapM_ reinitPP ds >> startupHook cf
      --, startupHook = mapM_ (restartP <* reinitPP)  ds >> startupHook cf
      -- reinitPP should be done before restart, because i may check or fill
      -- some PP values in startP.
      , startupHook = mapM_ (liftA2 (<*) reinitPP restartP) ds >> startupHook cf
      }
  where
    dockKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
    dockKeys        = fmap concat . sequence $
                        map toggleAllDocks ts ++ map toggleDock ds

reinitPP :: DockClass a => a -> X ()
reinitPP y          = withProcess (\x -> return (setDockPP (getDockPP y) x)) y

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

toggleAllDocks :: (ButtonMask, KeySym) -> XConfig l
               -> [((ButtonMask, KeySym), X ())]
toggleAllDocks (mk, k) XConfig {modMask = m} =
                        [((m .|. mk, k), sendMessage ToggleStruts)]

class ProcessClass a => DockClass a where
    dockToggleKey   :: a -> Maybe (ButtonMask, KeySym)
    getDockPP       :: a -> Maybe PP
    setDockPP       :: Maybe PP -> a -> a

toggleDock :: DockClass a => a -> XConfig l -> [((ButtonMask, KeySym), X ())]
toggleDock x (XConfig {modMask = m}) = maybeToList $ do
    (mk, k) <- dockToggleKey x
    return ((m .|. mk, k), toggleProcessStruts x)


-- Toggle struts for any ProcessClass type (in fact, i just need (Eq a) and
-- getPidP).
toggleProcessStruts :: ProcessClass a => a -> X ()
toggleProcessStruts = withProcess $ \x -> do
    maybe (return ()) togglePidStruts (getPidP x)
    return x

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

dockLog :: DockClass a => a ->  X ()
dockLog             = withProcess $ \x -> do
    maybe (return ()) dynamicLogWithPP (getDockPP x)
    return x


-- This XmobarPID definition suitable for launching several xmobars. They will
-- be distinguished by config file name.
data XmobarPID      = XmobarPID
                        { xmobarPID     :: First ProcessID
                        , xmobarConf    :: FilePath
                        , xmobarPP2     :: Maybe PP
                        , xmobarToggle  :: Maybe (ButtonMask, KeySym)
                        }
  deriving (Typeable)
-- Show and Read instances, which omit Handle.
instance Show XmobarPID where
    showsPrec d x   = showParen (d > app_prec) $
        showString "XmobarPID {xmobarPID = " . showsPrec d (xmobarPID x)
        . showString ", xmobarConf = " . showsPrec d (xmobarConf x)
        . showString ", xmobarToggle = " . showsPrec d (xmobarToggle x)
        . showString "}"
      where
        app_prec    = 10
instance Read XmobarPID where
    readsPrec d     = readParen (d > app_prec) . runStateT $ do
        readLexsM ["XmobarPID"]
        xp <- readLexsM ["{", "xmobarPID", "="] >> readsPrecM d
        xc <- readLexsM [",", "xmobarConf", "="] >> readsPrecM d
        xt <- readLexsM [",", "xmobarToggle", "="] >> readsPrecM d
        readLexsM ["}"]
        let x = XmobarPID
                  { xmobarPID       = xp
                  , xmobarConf      = xc
                  , xmobarPP2       = Nothing
                  , xmobarToggle    = xt
                  }
        return x
      where
        app_prec    = 10

instance Eq XmobarPID where
    XmobarPID {xmobarConf = xcf} == XmobarPID {xmobarConf = ycf}
      | xcf == ycf  = True
      | otherwise   = False
instance Monoid XmobarPID where
    mempty          = XmobarPID
                        { xmobarPID = First Nothing
                        , xmobarConf = ""
                        , xmobarPP2  = Nothing
                        , xmobarToggle = Nothing
                        }
    x `mappend` y   = x{xmobarPID = xmobarPID x `mappend` xmobarPID y}
instance ProcessClass XmobarPID where
    getPidP         = getFirst . xmobarPID
    setPidP mp' x   = x{xmobarPID = First mp'}
instance RestartClass XmobarPID where
    runP x@(XmobarPID{xmobarConf = xcf, xmobarPP2 = Just xpp}) = do
        (h, p) <- spawnPipe' "xmobar" [xcf]
        return (x{ xmobarPID = First (Just p)
                 , xmobarPP2 = Just (xpp{ppOutput = hPutStrLn h})
                 })
    runP x@(XmobarPID{xmobarConf = xcf})
      | otherwise   = defaultRunP "xmobar" [xcf] x
    killP           = return . resetPipe <=< defaultKillP
      where
        resetPipe :: XmobarPID -> XmobarPID
        resetPipe x@(XmobarPID{xmobarPP2 = Just xpp}) =
            x{xmobarPP2 = Just (xpp{ppOutput = const (return ())})}
        resetPipe x = x
xmobarPP' :: PP
xmobarPP' = xmobarPP {ppOutput = const (return ())}
instance DockClass XmobarPID where
    dockToggleKey   = xmobarToggle
    getDockPP       = xmobarPP2
    setDockPP pp x  = maybe x (\t -> x{xmobarPP2 = Just t}) pp


-- Copy from xmobar/src/Config.hs . I need to read xmobar config for
-- determining its position.


-- End copy from xmobar/src/Config.hs .

-- This TrayerPID definition allows to run only one trayer instance, bceause
-- all values of this type are equal.
newtype TrayerPID    = TrayerPID {trayerPID  :: First ProcessID}
  deriving (Show, Read, Typeable, Monoid)
instance Eq TrayerPID where
  _ == _    = True
instance ProcessClass TrayerPID where
  getPidP        = getFirst . trayerPID
  setPidP mp' x  = x{trayerPID = First mp'}
instance RestartClass TrayerPID where
  runP           = defaultRunP "trayer"
      [ "--edge", "top", "--align", "right"
      , "--SetDockType", "true", "--SetPartialStrut", "true"
      , "--expand", "true", "--width", "10"
      , "--transparent", "true" , "--tint", "0x191970"
      , "--height", "12"
      ]

