{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Docks.Trayer
    ( Trayer (..)
    , trayersList
    , defaultTrayer
    )
  where

import System.Posix.Types (ProcessID)

import XMonad
import Sgf.Control.Lens
import Sgf.XMonad.Restartable
import Sgf.XMonad.Docks

-- This Trayer definition allows to run only one trayer instance, bceause
-- all values of this type are equal.
newtype Trayer    = Trayer {_trayerPid  :: Maybe ProcessID}
  deriving (Show, Read, Typeable)
trayerPid :: Lens Trayer (Maybe ProcessID)
trayerPid f z@(Trayer {_trayerPid = x})
                    = fmap (\x' -> z{_trayerPid = x'}) (f x)
defaultTrayer :: Trayer
defaultTrayer       = Trayer {_trayerPid = Nothing}

instance Eq Trayer where
  _ == _    = True
instance ProcessClass Trayer where
  pidL              = trayerPid
instance RestartClass Trayer where
  runP           = defaultRunP "trayer"
      [ "--edge", "top", "--align", "right"
      , "--SetDockType", "true", "--SetPartialStrut", "true"
      , "--expand", "true", "--width", "10"
      , "--transparent", "true" , "--tint", "0x191970"
      , "--height", "12"
      ]
instance DockClass Trayer where
    dockToggleKey   = const Nothing
    ppL             = nothingL

-- Lens for obtaining list of all Xmobars stored in extensible state.
trayersList :: LensA (ListP Trayer) [Trayer]
trayersList         = processList

