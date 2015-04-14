
module Sgf.XMonad.Util.EZConfig
    ( mapKeys
    , addModMask
    )
  where

import XMonad
import qualified Data.Map as M

-- Map function over keys's Map .
mapKeys :: (M.Map (ButtonMask, KeySym) (X ()) -> M.Map (ButtonMask, KeySym) (X ()))
           -> XConfig a -> XConfig a
mapKeys f conf      = conf {keys = fmap f (keys conf)}

-- Define each key second time with different modMask (obtained by passing
-- current modMask to function). Suitable for use with mapKeys .
addModMask :: (ButtonMask -> ButtonMask)
                     -> M.Map (ButtonMask, KeySym) (X ())
                     -> M.Map (ButtonMask, KeySym) (X ())
addModMask f xs     = xs `M.union` M.mapKeys (\(m, t) -> (f m, t)) xs

