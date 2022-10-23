module Matrix (saddlePoints) where

import Data.Array (Array, (!))
import qualified Data.Array as A

saddlePoints :: Ord e => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix = filter (isSaddlePoint matrix) $ A.indices matrix

isSaddlePoint :: Ord e => Array (Int, Int) e -> (Int, Int) -> Bool
isSaddlePoint matrix i
  | A.inRange bounds i = maxOfRow matrix i && minOfCol matrix i
  | otherwise = False
  where bounds = A.bounds matrix

maxOfRow :: Ord e => Array (Int, Int) e -> (Int, Int) -> Bool
maxOfRow matrix (x, y) = all (\i -> (matrix ! i) <= (matrix ! (x, y))) $ rowIxs matrix x

rowIxs :: Ord e => Array (Int, Int) e -> Int -> [(Int, Int)]
rowIxs matrix x = [(x, j) | j <- [a..b]]
  where ((_, a), (_, b)) = A.bounds matrix

minOfCol :: Ord e => Array (Int, Int) e -> (Int, Int) -> Bool
minOfCol matrix (x, y) = all (\i -> (matrix ! i) >= (matrix ! (x, y))) $ colIxs matrix y

colIxs :: Ord e => Array (Int, Int) e -> Int -> [(Int, Int)]
colIxs matrix y = [(i, y) | i <- [a..b]]
  where ((a, _), (b, _)) = A.bounds matrix
