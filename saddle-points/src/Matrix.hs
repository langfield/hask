module Matrix (saddlePoints) where

import Data.Array

saddlePoints :: Ord a => Array (Int, Int) a -> [(Int, Int)]
saddlePoints a = (concatMap go . assocs) a
  where
    go ((x,y), u)
      | null maxInRow && null minInCol = [(x,y)]
      | otherwise = []
      where 
        maxInRow = [((i,j), v) | ((i,j), v) <- assocs a, i == x, v > u]
        minInCol = [((i,j), v) | ((i,j), v) <- assocs a, j == y, v < u]
