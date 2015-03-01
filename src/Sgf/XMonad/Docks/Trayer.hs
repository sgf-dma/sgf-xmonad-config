{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.XMonad.Docks.Trayer
    ( TrayerPID (..)
    )
  where

import Data.Monoid
import System.Posix.Types (ProcessID)

import XMonad
import Sgf.XMonad.Restartable

-- FIXME: Rename TrayerPID to Trayer.
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

