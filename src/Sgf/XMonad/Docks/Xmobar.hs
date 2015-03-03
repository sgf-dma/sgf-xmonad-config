{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.XMonad.Docks.Xmobar
    ( Xmobar
    , xmobarConf
    , xmobarPP2
    , xmobarToggle
    , defaultXmobar
    , xmobarPP'
    )
  where

import Data.Maybe
import Control.Monad.State
import System.IO
import System.Posix.Types (ProcessID)

import XMonad
import XMonad.Hooks.DynamicLog

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.XMonad.Util.Run (spawnPipe')
import Sgf.XMonad.Restartable
import Sgf.XMonad.Docks

-- This Xmobar definition suitable for launching several xmobars. They will
-- be distinguished by config file name.
data Xmobar      = Xmobar
                        { _xmobarPid     :: Maybe ProcessID
                        , _xmobarConf    :: FilePath
-- FIXME: Rename _xmobarPP2.
                        , _xmobarPP2     :: Maybe PP
                        , _xmobarToggle  :: Maybe (ButtonMask, KeySym)
                        }
  deriving (Typeable)
xmobarPid :: Lens Xmobar (Maybe ProcessID)
xmobarPid f z@(Xmobar {_xmobarPid = x})
                    = fmap (\x' -> z{_xmobarPid = x'}) (f x)
xmobarConf :: Lens Xmobar FilePath
xmobarConf f z@(Xmobar {_xmobarConf = x})
                    = fmap (\x' -> z{_xmobarConf = x'}) (f x)
xmobarPP2 :: Lens Xmobar (Maybe PP)
xmobarPP2 f z@(Xmobar {_xmobarPP2 = x})
                    = fmap (\x' -> z{_xmobarPP2 = x'}) (f x)
xmobarToggle :: Lens Xmobar (Maybe (ButtonMask, KeySym))
xmobarToggle f z@(Xmobar {_xmobarToggle = x})
                    = fmap (\x' -> z{_xmobarToggle = x'}) (f x)
-- FIXME: Does this good default?
defaultXmobar :: Xmobar
defaultXmobar       = Xmobar
                        { _xmobarPid    = Nothing
                        , _xmobarConf   = ".xmobarrc"
                        , _xmobarPP2    = Just xmobarPP
                        , _xmobarToggle = Nothing
                        }

-- Show and Read instances omiting some non-showable/non-readable records.
instance Show Xmobar where
    showsPrec d x   = showParen (d > app_prec) $
        showString "Xmobar {_xmobarPid = "  . showsPrec d (view xmobarPid x)
        . showString ", _xmobarConf = "     . showsPrec d (view xmobarConf x)
        . showString ", _xmobarToggle = "   . showsPrec d (view xmobarToggle x)
        . showString "}"
      where
        app_prec    = 10
instance Read Xmobar where
    readsPrec d     = readParen (d > app_prec) . runStateT $ do
        readLexsM ["Xmobar"]
        xp <- readLexsM ["{", "_xmobarPid", "="] >> readsPrecM d
        xc <- readLexsM [",", "_xmobarConf", "="] >> readsPrecM d
        xt <- readLexsM [",", "_xmobarToggle", "="] >> readsPrecM d
        readLexsM ["}"]
        let x = set xmobarPid xp
                  . set xmobarConf xc
                  . set xmobarToggle xt
                  $ defaultXmobar
        return x
      where
        app_prec    = 10

instance Eq Xmobar where
    x == y
      | view xmobarConf x == view xmobarConf y = True
      | otherwise   = False
instance ProcessClass Xmobar where
    pidL            = xmobarPid
instance RestartClass Xmobar where
    runP x
      | isJust (view xmobarPP2 x) = do
          (h, p) <- spawnPipe' "xmobar" [view xmobarConf x]
          let xpp' = do
                       xpp <- view xmobarPP2 x
                       return (xpp{ppOutput = hPutStrLn h})
          return
            . set xmobarPid (Just p)
            . set xmobarPP2 xpp'
            $ x
      | otherwise   = defaultRunP "xmobar" [view xmobarConf x] x
    killP           = return . resetPipe <=< defaultKillP
      where
        resetPipe :: Xmobar -> Xmobar
        resetPipe x@(Xmobar{_xmobarPP2 = Just xpp}) =
            x{_xmobarPP2 = Just (xpp{ppOutput = const (return ())})}
        resetPipe x = x
-- FIXME: Repeats resetPipe.
xmobarPP' :: PP
xmobarPP' = xmobarPP {ppOutput = const (return ())}
instance DockClass Xmobar where
    dockToggleKey   = view xmobarToggle
    ppL             = xmobarPP2

