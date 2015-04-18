{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.VNC
    ( isVnc
    , handleVnc
    )
  where

import Data.List
import System.Process
import System.Environment

import XMonad

import Sgf.XMonad.Util.EZConfig


-- If xmonad has run by VNC server, redefine each key with controlMask added
-- to modMask. This allows using remote session with xmonad from xmonad on
-- local host, if they have the same config: xmonad running on local host does
-- not define keys with controlMask, but remote xmonad does, so i can interact
-- with local host's xmonad using regular modMask, and with remote one using
-- Ctrl + regular modMask. Note, that if local host's X server has VNC
-- extension loaded, then local host xmonad will define keys with Ctrl too.
-- After all, if local host's xmonad can be accessed remotely, it may need
-- this feature.  This means, that this function does not help, when remote
-- xmonad is started from (potentially) shared session (at least through vnc).
-- But i may use different methods for sharing session, than VNC (e.g.
-- x11vnc).
handleVnc :: LayoutClass l Window => XConfig l -> IO (XConfig l)
handleVnc xcf       = do
    b <- isVnc
    if b
      then return (mapKeys (addModMask (controlMask .|.)) xcf)
      else return xcf

-- Try to guess, has xmonad run by VNC server: i'll check whether X11 VNC
-- extension (used by TigerVNC) is loaded or XMONAD_VNC environment variable
-- is set (may be set from ~/.vnc/xstartup).
isVnc :: IO Bool
isVnc     = do
    uninstallSignalHandlers
    c <- readProcess "xdpyinfo" [] ""
    xs <- getEnvironment
    return $ any id
      [ elem "VNC-EXTENSION" (x11Extensions c)
      , elem "XMONAD_VNC" (map fst xs)
      ]
  where
    -- Extract loaded X11 extensions from `xdpyinfo` output.
    x11Extensions :: String -> [String]
    x11Extensions c = 
        let l = map words
                    . dropWhile (not . isPrefixOf "number of extensions:")
                    . lines $ c
            n = (\("number" : "of" : "extensions:" : ns : []) -> read ns)
                    . head $ l
        in  concat . take n . drop 1 $ l

