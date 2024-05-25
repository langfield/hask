module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.Map.Strict ((!?), fromAscList)
import Data.Maybe (listToMaybe, fromMaybe)

neighbors :: a -> [[a]] -> [[[a]]]
neighbors d xss = map (map nbs) coords
  where
    (m,n) = (length xss, maybe 0 length (listToMaybe xss))
    coords = [[(i,j) | j <- [0..n-1]] | i <- [0..m-1]]
    directions = (0,0) : [(dx,dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
    values = fromAscList $ zip (concat coords) (concat xss)
    nb (x,y) (dx,dy) = fromMaybe d (values !? (x+dx,y+dy))
    nbs xy = map (nb xy) directions

annotate :: [String] -> [String]
annotate = map (map go) . neighbors ' '
  where
    go [] = ' '
    go ('*':_) = '*'
    go (_:xs) =
      case length $ filter (== '*') xs of
        0 -> ' '
        n -> intToDigit n
