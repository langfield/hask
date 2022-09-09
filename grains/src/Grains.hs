module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n <= 0 = Nothing
  | otherwise = 2 ^ n

-- How can we ever get back to ``Integer`` from ``Maybe Integer`` without
-- panicking?
total :: Integer
total = sum $ fmap square [1..64]
