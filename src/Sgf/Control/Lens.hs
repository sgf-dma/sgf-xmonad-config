{-# LANGUAGE Rank2Types #-}

module Sgf.Control.Lens
    ( Lens
    , view
    , set
    , modify
    )
  where

import Control.Applicative
import Control.Monad.Identity

-- Redefine Lenses.
type Lens a b       = forall f. Functor f => (b -> f b) -> a -> f a

view :: Lens a b -> a -> b
view l              = getConst . l Const

modify :: Lens a b -> (b -> b) -> a -> a
modify l f          = runIdentity . l (Identity . f)

set :: Lens a b -> b -> a -> a
set l s             = modify l (const s)

