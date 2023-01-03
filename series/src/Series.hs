module Series (slices) where

import qualified Data.Char as C

slices :: Int -> String -> [[Int]]
slices 0 xs = replicate (length xs + 1) []
slices _ "" = []
slices n xs@(_:xs')
  | n' == n   = slice : slices n xs'
  | otherwise = []
  where
    slice = map C.digitToInt $ take n xs
    n' = length slice
