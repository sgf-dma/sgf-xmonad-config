
module Sgf.Data.List
    ( insertWith
    , readLexs
    , anyP
    )
  where

import Control.Monad.Instances

-- Similar to insertWith from Data.Map, but for lists.
insertWith :: Eq a => (a -> a -> a) -> a -> [a] -> [a]
insertWith f y []   = [y]
insertWith f y (x : xs)
  | y == x          = f y x : xs
  | otherwise       = x : insertWith f y xs

readLexs :: [String] -> String -> [String]
readLexs [] ys       = [ys]
readLexs (x : xs) ys = do
                        (z, zs) <- lex ys
                        if x == z then readLexs xs zs else []

anyP :: [a -> Bool] -> a -> Bool
anyP fs             = any id . sequence fs
