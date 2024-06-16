module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue _ [] = 0
maximumValue n ((wt,val):rest)
  | wt <= n = max (val + maximumValue (n-wt) rest) (maximumValue n rest)
  | otherwise = maximumValue n rest
