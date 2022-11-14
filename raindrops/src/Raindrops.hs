module Raindrops (convert) where

-- Factor out all powers of `k` from `n`.
factor :: Int -> Int -> Int
factor _ 1 = 1
factor _ (-1) = -1
factor k n
  | n `mod` k == 0 = factor k (n `div` k)
  | otherwise = n

convert :: Int -> String
convert n
  | n `mod` 3 == 0 || n `mod` 5 == 0 || n `mod` 7 == 0 = convertGood n
  | otherwise = show n

convertGood :: Int -> String
convertGood n
  | n `mod` 3 == 0 = "Pling" ++ (convertGood . factor 3) n
  | n `mod` 5 == 0 = "Plang" ++ (convertGood . factor 5) n
  | n `mod` 7 == 0 = "Plong" ++ (convertGood . factor 7) n
  | otherwise = ""
