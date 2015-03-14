{-# LANGUAGE Rank2Types #-}

module Sgf.Control.Lens
    ( Lens
    , view
    , set
    , modify
    , LensA
    , viewA
    , viewAmaybe
    , setA
    , modifyA
    , modifyAA
    , maybeL
    , nothingL
    )
  where

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Identity

-- Redefine Lenses.
type Lens a b       = forall f. Functor f => (b -> f b) -> a -> f a

view :: Lens a b -> a -> b
view l              = getConst . l Const

modify :: Lens a b -> (b -> b) -> a -> a
modify l f          = runIdentity . l (Identity . f)

set :: Lens a b -> b -> a -> a
set l s             = modify l (const s)


type LensA a b      = forall f. Applicative f => (b -> f b) -> a -> f a

viewA :: LensA a b -> a -> b
viewA l             = fromJust . getLast . getConst . l (Const . Last . Just)
viewAmaybe :: LensA a b -> a -> Maybe b
viewAmaybe l        = getLast . getConst . l (Const . Last . Just)

modifyA :: LensA a b -> (b -> b) -> a -> a
modifyA l f         = runIdentity . l (Identity . f)

modifyAA :: Applicative t => LensA a b -> (b -> t b) -> a -> t a
modifyAA l f        = runIdentityT . l (IdentityT . f)

setA :: LensA a b -> b -> a -> a
setA l s            = modifyA l (const s)

-- Lens to value in Maybe. If there is Nothing, original value in Functor
-- returned. Thus, i need Applicative.
maybeL :: LensA (Maybe a) a
maybeL f x          = maybe (pure x) (fmap Just . f) x

-- Lens from some type to Maybe, which always sees Nothing and ignores changes
-- of Maybe record.  It may be used for value, which does not have (Maybe a)
-- record at all.
nothingL :: LensA a (Maybe b)
nothingL f x        = fmap (const x) (f Nothing)

