module Minesweeper (annotate, threeSum) where

import Data.Char (intToDigit)
import Data.List (transpose)

annotate :: [String] -> [String]
annotate [""] = [""]
annotate xs =
  let numberMap = map starToInt <$> xs
      rowSums = threeSum <$> numberMap
      allSums = transpose $ threeSum <$> transpose rowSums
   in zipWith (zipWith intToChar) allSums xs

-- | Take the sum of each element and its two neighbors.
threeSum :: [Int] -> [Int]
threeSum [] = []
threeSum (x:xs) = go (0,x) xs
  where
    go (l,c) [] = [l+c]
    go (l,c) (r:rs) = (l+c+r) : go (c,r) rs

starToInt :: Char -> Int
starToInt '*' = 1
starToInt _ = 0

intToChar :: Int -> Char -> Char
intToChar _ '*' = '*'
intToChar 0 _ = ' '
intToChar n _ = intToDigit n
