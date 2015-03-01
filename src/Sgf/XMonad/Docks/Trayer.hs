{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.XMonad.Docks.Trayer
    ( Trayer (..)
    )
  where

import Data.Monoid
import System.Posix.Types (ProcessID)

import XMonad
import Sgf.XMonad.Restartable

-- This Trayer definition allows to run only one trayer instance, bceause
-- all values of this type are equal.
newtype Trayer    = Trayer {trayerPID  :: First ProcessID}
  deriving (Show, Read, Typeable, Monoid)
instance Eq Trayer where
  _ == _    = True
instance ProcessClass Trayer where
  getPidP        = getFirst . trayerPID
  setPidP mp' x  = x{trayerPID = First mp'}
instance RestartClass Trayer where
  runP           = defaultRunP "trayer"
      [ "--edge", "top", "--align", "right"
      , "--SetDockType", "true", "--SetPartialStrut", "true"
      , "--expand", "true", "--width", "10"
      , "--transparent", "true" , "--tint", "0x191970"
      , "--height", "12"
      ]

