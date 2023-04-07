module Alphametics (solve) where

import Debug.Trace (trace)

ttt :: Show a => String -> a -> a
ttt s x = trace (s ++ ": " ++ show x) x

splitAtEq :: String -> (String, String)
splitAtEq s = (expression, result)
  where
    s' = reverse s
    result = (reverse . takeWhile (/= ' ')) s'
    expression = (reverse . dropWhile (`elem` " =") . dropWhile (/= ' ')) s'

solve :: String -> Maybe [(Char, Int)]
solve puzzle = Nothing
