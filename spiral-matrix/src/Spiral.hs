module Spiral (spiral) where

import Data.Array ((//), Array)
import Data.Set (Set)

import qualified Data.Array as A
import qualified Data.Set as S

import Debug.Trace (trace)

ttt :: Show a => String -> a -> a
ttt s x = trace (s ++ ": " ++ show x) x

type Coord = (Int, Int)
type Matrix = Array Coord Int
type Visited = Set Coord

data Direction = North | South | East | West deriving (Eq, Show)

turn :: Direction -> Direction
turn East  = South
turn South = West
turn West  = North
turn North = East

move :: Direction -> Coord -> Coord
move East  (x, y) = (x, y + 1)
move South (x, y) = (x + 1, y)
move West  (x, y) = (x, y - 1)
move North (x, y) = (x - 1, y)

chunk :: Int -> [Int] -> [[Int]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

spiral :: Int -> [[Int]]
spiral n = (chunk n . A.elems) filled
  where
    (lb, ub) = ((0, 0), (n - 1, n - 1))
    empty    = A.listArray (lb, ub) $ replicate (n ^ (2 :: Int)) 0
    (_, _, filled, _) =
      foldr go ((0, 0), East, empty, S.empty) (reverse [1 .. n ^ (2 :: Int)])

    inBounds :: Coord -> Bool
    inBounds (x, y) = 0 <= x && x <= n - 1 && 0 <= y && y <= n - 1

    next :: Direction -> Visited -> Coord -> (Direction, Coord)
    next d visited pos
      | inBounds pos' && not (pos' `S.member` visited) = (d, pos')
      | otherwise = (d', move d' pos)
      where
        pos' = move d pos
        d' = turn d

    update :: Matrix -> (Coord, Int) -> Matrix
    update m (pos, k)
      | inBounds pos = m // [(pos, k)]
      | otherwise = error ("bad index: " ++ show pos)

    go :: Int
       -> (Coord, Direction, Matrix, Visited)
       -> (Coord, Direction, Matrix, Visited)
    go k (pos, d, arr, visited) =
      ((x', y'), d', update arr (pos, k), S.insert pos visited)
      where (d', (x', y')) = next d visited pos
