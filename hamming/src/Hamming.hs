module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance [] _ = Nothing
distance _ [] = Nothing
distance (x : xs) (y : ys)
  | x == y = distance xs ys
  | otherwise = (+1) <$> distance xs ys
  | otherwise = foldr (fmap (+1)) 0
