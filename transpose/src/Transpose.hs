module Transpose (transpose) where

import qualified Data.List as L

transpose :: [String] -> [String]
transpose ls =
  (snd . foldr go (0, []) . chunksOf m . map snd . L.sort . concat) assocs
  where
    m    = length ls
    n    = foldr (max . length) 0 ls
    rows = map (pad n) ls
    assocs =
      [ [ ((j, i), c) | (j, c) <- zip [(1 :: Int) ..] row ]
      | (i, row) <- zip [(1 :: Int) ..] rows
      ]

    pad :: Int -> String -> String
    pad k s
      | length s < k = s ++ replicate (k - length s) ' '
      | otherwise    = s

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf k xs = take k xs : chunksOf k (drop k xs)

    trim :: String -> String
    trim s = (reverse . dropWhile (== ' ') . reverse) s

    go :: String -> (Int, [String]) -> (Int, [String])
    go x (k, xs) = (k', take k' x : xs) where k' = max k $ (length . trim) x
