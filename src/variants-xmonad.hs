{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts  #-}

import XMonad
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Maximize
import XMonad.Util.Run (spawnPipe)

import Codec.Binary.UTF8.String
import System.Posix.Process
import System.IO
import System.Posix.IO
import DBus
import DBus.Client
import qualified XMonad.Util.ExtensibleState as XS
import Control.Monad
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86 -- For media keys.
import qualified Data.Map as M
import Control.Applicative
import System.IO (hPutStrLn)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Process
import System.Posix.Types

main :: IO ()
main                = do
    -- FIXME: Spawn process directly, not through shell.
    --runFehBg
    --_ <- runTrayer
    --xmPipe <- spawnPipe ("xmobar ~/.xmobarrc")
    xmonad
      . alterKeys myKeys
      $ defaultConfig
          { manageHook = manageDocks <+> testVlc <+> manageHook defaultConfig
          , layoutHook = smartBorders . avoidStruts  $  myLayout
          --, layoutHook = lessBorders OtherIndicated $ avoidStruts  $  myLayout
          --, layoutHook = avoidStruts  $  myLayout
          , logHook    = traceXS >> logToXmobar
          --, startupHook = runP xmobarP >> return ()
          , startupHook = traceXS >> restartXmobar2'
          , modMask = mod4Mask
	  --, handleEventHook = toggleDocksHook 30 xK_v <+> myDocksEventHook
	  , handleEventHook = myDocksEventHook
          --, modMask = controlMask
          , focusFollowsMouse = False
          , terminal = "xterm -fg black -bg white"
          --, layoutHook = smartBorders $ layoutHook xfceConfig
          }

spawnPipe' :: [String] -> X (Handle, ProcessID)
spawnPipe' (x : xs) = io $ do
        (rd, wr) <- createPipe
        setFdOption wr CloseOnExec True
        h <- fdToHandle wr
        hSetBuffering h LineBuffering
        p <- xfork $ do
              _ <- dupTo rd stdInput
              --executeFile "/bin/sh" False ["-c", encodeString x] Nothing
              executeFile x False xs Nothing
        closeFd rd
        return (h, p)

logToXmobar :: X ()
logToXmobar = do
    (XmobarHandle m) <- XS.get
    whenJust m $ \xmPipe -> 
        dynamicLogWithPP xmobarPP
                           { ppOutput = hPutStrLn xmPipe
                           , ppTitle  = xmobarColor "green" ""
                                          . shorten 50
                           }


traceXS :: X ()
traceXS = do
    trace "Abc"
    mp <- XS.get
    whenJust (view (mp :: XmobarPID2)) $ trace . show


{-
-- Variant 1: Data type and (Maybe a) instance.

traceXS :: X ()
traceXS = do
    trace "Abc"
    mp <- XS.get
    whenJust (mp :: Maybe XmobarPID) $ trace . show

data Restartable a = Restartable
                      { killP :: a -> X ()
                      , runP  :: X a
                      }
restartP :: (ExtensionClass (Maybe a)) => Restartable a -> X ()
restartP r    = do
  mp <- XS.get
  whenJust mp (killP r)
  p' <- runP r
  XS.put (Just p')
instance (Show a, Read a, Typeable a) => ExtensionClass (Maybe a) where
   initialValue   = Nothing
   extensionType  = PersistentExtension

-- For data type..
newtype XmobarPID = XmobarPID ProcessID
  deriving (Show, Read, Typeable)

newtype XmobarHandle = XmobarHandle (Maybe Handle)
  deriving (Typeable)

instance ExtensionClass XmobarHandle where
    initialValue  = XmobarHandle Nothing

xmobarP :: Restartable XmobarPID
xmobarP   = Restartable killXmobar runXmobar
  where
    killXmobar :: XmobarPID -> X ()
    killXmobar (XmobarPID p)  = io $ spawn ("kill " ++ show p)
    runXmobar :: X XmobarPID
    runXmobar     = do
      (h, p) <- spawnPipe' ["/usr/bin/xmobar", "/home/dmitriym/.xmobarrc"]
      XS.put (XmobarHandle (Just h))
      return (XmobarPID p)

restartXmobar :: X ()
restartXmobar     = restartP xmobarP
-}

{-
-- Variant 2. Class and (Maybe a) instance.

traceXS :: X ()
traceXS = do
    trace "Abc"
    mp <- XS.get
    whenJust (mp :: Maybe XmobarPID) $ trace . show

class RestartClass a where
  killP' :: a -> X ()
  runP'  :: X a
restartP' :: (ExtensionClass (Maybe a), RestartClass a) => X a
restartP'     = do
  mp <- XS.get
  whenJust mp killP'
  p' <- runP'
  XS.put (Just p' `asTypeOf` mp)
  return p'
instance (Show a, Read a, Typeable a) => ExtensionClass (Maybe a) where
   initialValue   = Nothing
   extensionType  = PersistentExtension

-- For data type..
newtype XmobarPID = XmobarPID ProcessID
  deriving (Show, Read, Typeable)

newtype XmobarHandle = XmobarHandle (Maybe Handle)
  deriving (Typeable)

instance ExtensionClass XmobarHandle where
    initialValue  = XmobarHandle Nothing

instance RestartClass XmobarPID where
  killP' (XmobarPID p) = io $ spawn ("kill " ++ show p)
  runP'                = do
      (h, p) <- spawnPipe' ["/usr/bin/xmobar", "/home/dmitriym/.xmobarrc"]
      XS.put (XmobarHandle (Just h))
      return (XmobarPID p)

restartXmobar' :: X ()
restartXmobar'    = do
      p <- restartP'
      let _ = p `asTypeOf` XmobarPID undefined
      return ()

-}

{-
-- Variant 3. Data type with Maybe and Lens a (Maybe ..).

traceXS :: X ()
traceXS = do
    trace "Abc"
    mp <- XS.get
    whenJust (view (mp :: XmobarPID2)) $ trace . show

data Restartable a = Restartable
                      { killP :: a -> X ()
                      , runP  :: X a
                      }
newtype XmobarPID = XmobarPID ProcessID
  deriving (Show, Read, Typeable)

newtype XmobarHandle = XmobarHandle (Maybe Handle)
  deriving (Typeable)

instance ExtensionClass XmobarHandle where
    initialValue  = XmobarHandle Nothing


instance (Show a, Read a, Typeable a) => ExtensionClass (Maybe a) where
   initialValue   = Nothing
   extensionType  = PersistentExtension


-- For data type..
xmobarP :: Restartable XmobarPID
xmobarP   = Restartable killXmobar runXmobar
  where
    killXmobar :: XmobarPID -> X ()
    killXmobar (XmobarPID p)  = io $ spawn ("kill " ++ show p)
    runXmobar :: X XmobarPID
    runXmobar     = do
      (h, p) <- spawnPipe' ["/usr/bin/xmobar", "/home/dmitriym/.xmobarrc"]
      XS.put (XmobarHandle (Just h))
      return (XmobarPID p)

newtype XmobarPID2 = XmobarPID2 (Maybe ProcessID)
  deriving (Typeable, Show, Read)

instance ExtensionClass XmobarPID2 where
    initialValue  = XmobarPID2 Nothing
    extensionType = PersistentExtension

class Lens a b | a -> b where
    view :: a -> b
    set  :: b -> a -> a

instance Lens XmobarPID2 (Maybe XmobarPID) where
    view (XmobarPID2 x)           = fmap XmobarPID x
    set (Just (XmobarPID x)) _    = XmobarPID2 (Just x)
    set Nothing  z                = z
restartP2 :: (ExtensionClass a, Lens a (Maybe b)) => Restartable b -> X a
restartP2 r       = do
  mp <- XS.get
  whenJust (view mp) (killP r)
  p' <- runP r
  let mp' = set (Just p') mp
  XS.put mp'
  return mp'

restartXmobar2 :: X ()
restartXmobar2    = do
  p <- restartP2 xmobarP
  let _ = p `asTypeOf` XmobarPID2 undefined
  return ()
-}


{-
-- Variant 4. RestartClass with Lens.

traceXS :: X ()
traceXS = do
    trace "Abc"
    mp <- XS.get
    whenJust (view (mp :: XmobarPID2)) $ trace . show

class RestartClass a where
  killP' :: a -> X ()
  runP'  :: X a

newtype XmobarPID = XmobarPID ProcessID
  deriving (Show, Read, Typeable)

newtype XmobarHandle = XmobarHandle (Maybe Handle)
  deriving (Typeable)

instance ExtensionClass XmobarHandle where
    initialValue  = XmobarHandle Nothing

-- For type-class..
instance RestartClass XmobarPID where
  killP' (XmobarPID p) = io $ spawn ("kill " ++ show p)
  runP'                = do
      (h, p) <- spawnPipe' ["/usr/bin/xmobar", "/home/dmitriym/.xmobarrc"]
      XS.put (XmobarHandle (Just h))
      return (XmobarPID p)

newtype XmobarPID2 = XmobarPID2 (Maybe ProcessID)
  deriving (Typeable, Show, Read)

instance ExtensionClass XmobarPID2 where
    initialValue  = XmobarPID2 Nothing
    extensionType = PersistentExtension

class Lens a b | a -> b where
    view :: a -> b
    set  :: b -> a -> a

instance Lens XmobarPID2 (Maybe XmobarPID) where
    view (XmobarPID2 x)           = fmap XmobarPID x
    set (Just (XmobarPID x)) _    = XmobarPID2 (Just x)
    set Nothing  z                = z

restartP2' :: (ExtensionClass a, Lens a (Maybe b), RestartClass b) => X a
restartP2'      = do
    mp <- XS.get
    whenJust (view mp) killP'
    p' <- runP'
    let mp' = set (Just p') mp
    XS.put mp'
    return mp'

restartXmobar2' :: X ()
restartXmobar2'   = do
  p <- restartP2'
  let _ = p `asTypeOf` XmobarPID2 undefined
  return ()
-}







-- Layouts definition from defaultConfig with Full layout without borders.
myLayout = maximize tiled ||| Mirror tiled ||| noBorders Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

testVlc :: ManageHook
testVlc	    = className =? "Vlc" --> doShift "7"

myDocksEventHook :: Event -> X All
myDocksEventHook e = do
    when (et == mapNotify || et == unmapNotify) $
        whenX ((not `fmap` (isClient w)) <&&> runQuery checkDock w) refresh
    return (All True)
    where w  = ev_window e
          et = ev_event_type e

data DockToggleTime = DTT { lastTime :: Time } deriving (Eq, Show, Typeable)

instance ExtensionClass DockToggleTime where
    initialValue = DTT 0

toggleDocksHook :: Int -> KeySym -> Event -> X All
toggleDocksHook to ks ( KeyEvent { ev_event_display = d
                                 , ev_event_type    = et
                                 , ev_keycode       = ekc
                                 , ev_time          = etime
                                 } ) =
        io (keysymToKeycode d ks) >>= toggleDocks >> return (All True)
    where
    toggleDocks kc
        | ekc == kc && et == keyPress = do
            safeSendSignal ["Reveal 0", "TogglePersistent"]
            XS.put ( DTT etime )
        | ekc == kc && et == keyRelease = do
            gap <- XS.gets ( (-) etime . lastTime )
            safeSendSignal [ "TogglePersistent"
                           , "Hide " ++ show (if gap < 400 then to else 0)
                           ]
        | otherwise = return ()

    safeSendSignal s = catchX (io $ sendSignal s) (return ())
    sendSignal    = withSession . callSignal
    withSession mc = connectSession >>= \c -> callNoReply c mc >> disconnect c
    callSignal :: [String] -> MethodCall
    callSignal s = ( methodCall
                     ( objectPath_    "/org/Xmobar/Control" )
                     ( interfaceName_ "org.Xmobar.Control"  )
                     ( memberName_    "SendSignal"          )
                   ) { methodCallDestination = Just $ busName_ "org.Xmobar.Control"
                     , methodCallBody        = map toVariant s
                     }

toggleDocksHook _ _ _ = return (All True)



-- Run trayer.
runTrayer :: IO ProcessHandle
runTrayer           = do
    let tray = proc "trayer"
                 [ "--edge", "top", "--align", "right"
                 , "--SetDockType", "true", "--SetPartialStrut", "true"
                 , "--expand", "true", "--width", "10"
                 , "--transparent", "true" , "--tint", "0x191970"
                 , "--height", "12"
                 ]
    (_, _, _, p) <- createProcess tray
    return p

-- Set up background image using feh.
runFehBg :: IO ()
runFehBg           = spawn "xsetroot -blue"

-- Union my keys config with current one in ((->) XConfig Layout) applicative
-- functor. Union prefers left argument, when duplicate keys are found, thus
-- my should go first.
alterKeys :: (XConfig Layout -> M.Map (ButtonMask, KeySym) (X ()))
             -> XConfig l -> XConfig l
alterKeys myKs cf@(XConfig {keys = ks}) = cf {keys = M.union <$> myKs <*> ks}

-- My key bindings. They are intended for use with alterKeys only.
myKeys :: XConfig l -> M.Map (ButtonMask, KeySym) (X ())
myKeys (XConfig {modMask = m}) =
    M.fromList
      [ 
      --((m .|. shiftMask, xK_p), spawn "exec gmrun")
        ((m .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((m , xK_m), withFocused (sendMessage . maximizeRestore))
      , ((m , xK_b), sendMessage (ToggleStrut U))
      , ((m , xK_v), return ())
      --, ((m , xK_n), withWindowSet (\wset -> ...))
      , ((0,           xK_Print), spawn "scrot")

      -- Audio keys.
      , ((0,     xF86XK_AudioLowerVolume), spawn "amixer set Master 1311-")
      -- FIXME: This really not exactly what i want. I want, that if sound is
      -- muted, one VolUp only unmutes it. Otherwise, just VolUp-s.
      , ((0,     xF86XK_AudioRaiseVolume), spawn $ "amixer set Master unmute; "
          					 ++ "amixer set Master 1311+")
      , ((0,     xF86XK_AudioMute       ), spawn "amixer set Master mute")
      ]

