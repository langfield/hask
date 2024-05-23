module Minesweeper (annotate, threeSum) where

import Data.Char (intToDigit)
import Data.List (transpose)

annotate :: [String] -> [String]
annotate [""] = [""]
annotate xs = fromCounts . sumCounts . toCounts $ xs
  where
    fromCounts = zipWith (zipWith intToChar) xs
    sumCounts = transpose . map threeSum . transpose . map threeSum

-- | Take the sum of each element and its two neighbors.
threeSum :: [Int] -> [Int]
threeSum [] = []
threeSum (x:xs) = go (0,x) xs
  where
    go (l,c) [] = [l+c]
    go (l,c) (r:rs) = (l+c+r) : go (c,r) rs

toCounts :: [[Char]] -> [[Int]]
toCounts = map (map go)
  where
    go '*' = 1
    go _   = 0

intToChar :: Char -> Int -> Char
intToChar '*' _ = '*'
intToChar _ 0 = ' '
intToChar _ n = intToDigit n
