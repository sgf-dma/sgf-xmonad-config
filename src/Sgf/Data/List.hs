
module Sgf.Data.List
    ( insertWith
    , ReadM
    , liftRead
    , readsPrecM
    , readLexs
    , readLexsM
    , anyP
    )
  where

import Control.Monad.State

-- Similar to insertWith from Data.Map, but for lists.
insertWith :: Eq a => (a -> a -> a) -> a -> [a] -> [a]
insertWith f y []   = [y]
insertWith f y (x : xs)
  | y == x          = f y x : xs
  | otherwise       = x : insertWith f y xs

-- Store remaining string in State monad.
type ReadM a        = StateT String [] a

-- Lift ReadS to ReadM .
liftRead :: Read a => (ReadS a) -> ReadM a
liftRead h = do
    r0 <- get
    (x, r1) <- lift (h r0)
    put r1
    return x

readsPrecM :: Read a => Int -> ReadM a
readsPrecM      = liftRead . readsPrec

-- Try to read specified lexems in turn and throw them away (thus, returning
-- remaining part of string).
readLexs :: [String] -> ReadS ()
readLexs [] ys       = [((), ys)]
readLexs (x : xs) ys = do
                        (z, zs) <- lex ys
                        if x == z then readLexs xs zs else []

readLexsM :: [String] -> ReadM ()
readLexsM           = liftRead . readLexs


anyP :: [a -> Bool] -> a -> Bool
anyP fs             = any id . sequence fs
