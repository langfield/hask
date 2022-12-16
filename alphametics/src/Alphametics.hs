module Alphametics (solve) where

import Debug.Trace (trace)

ttt :: Show a => String -> a -> a
ttt s x = trace (s ++ ": " ++ show x) x

solve :: String -> Maybe [(Char, Int)]
solve puzzle = Just [(c, 0)]
  where
    c :: Char
    c =
      case ttt "puzzle" puzzle of
        "" -> 'a'
        (x:_) -> x
