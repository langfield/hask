module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.Map.Strict ((!?), fromAscList)
import Data.Maybe (listToMaybe, fromMaybe)

neighbors :: a -> [[a]] -> [[[a]]]
neighbors d zss = unmaybe [[map (vals !?) (nbs x y) | y <- [0..n-1]] | x <- [0..m-1]]
  where
    unmaybe = (map . map . map) (fromMaybe d)
    (m, n) = (length zss, maybe 0 length (listToMaybe zss))
    nbs x y = (x,y) : [(i,j) | i <- [x-1..x+1], j <- [y-1..y+1], (i,j) /= (x,y)]
    vals = fromAscList [((i,j),z) | (i,zs) <- zip [0..] zss, (j,z) <- zip [0..] zs]

annotate :: [String] -> [String]
annotate = map (map go) . neighbors ' '
  where
    go [] = ' '
    go ('*':_) = '*'
    go (_:xs) =
      case length $ filter (== '*') xs of
        0 -> ' '
        n -> intToDigit n
