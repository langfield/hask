module GameOfLife (tick) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map.Strict (fromAscList, (!?))

neighbors :: a -> [[a]] -> [[[a]]]
neighbors d xss = map (map nbs) coords
  where
    (m,n) = (length xss, maybe 0 length (listToMaybe xss))
    coords = [[(i,j) | j <- [0..n-1]] | i <- [0..m-1]]
    vals = fromAscList $ zip (concat coords) (concat xss)
    directions = (0,0) : [(dx,dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
    nbs (x,y) = map (\(dx,dy) -> fromMaybe d (vals !? (x+dx,y+dy))) directions 

step :: [Int] -> Int
step [] = 0
step (x:xs) = go x $ length $ filter (== 1) xs
  where
    go 1 2 = 1
    go 1 3 = 1
    go 0 3 = 1
    go _ _ = 0

tick :: [[Int]] -> [[Int]]
tick = map (map step) . neighbors 0
