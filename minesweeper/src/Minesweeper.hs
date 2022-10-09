module Minesweeper (annotate) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, fromListWith)
import qualified Data.Map as M
import Data.Char (intToDigit)


-- Annotate a minesweeper board.
annotate :: [String] -> [String]
annotate board
  | m == 0 = []
  | n == 0 = board
  | otherwise = map (annotateRow board n adjacentMineCountMap) [0..m]
  where m = length board
        n = length $ board !! 0
        squares = [(x, y) | x <- [0..m], y <- [0..n]]
        adjacentMineCounts = map (\s -> (s, countAdjacentMines board m n s)) squares
        adjacentMineCountMap = fromListWith (\a b -> a + b) adjacentMineCounts


-- Fill in mine counts for a row of the board.
annotateRow :: [String] -> Int -> Map (Int, Int) Int -> Int -> String
annotateRow board n adjacentMineCountMap x = map (annotateSquare board adjacentMineCountMap x) [0..n]


-- Fill in mine counts for a single square.
annotateSquare :: [String] -> Map (Int, Int) Int -> Int -> Int -> Char
annotateSquare board adjacentMineCountMap x y
  | isMine board (x, y) = '*'
  | otherwise = intToDigit $ adjacentMineCountMap M.! (x, y)


-- Count mines adjacent to (x, y) given board size.
countAdjacentMines :: [String] -> Int -> Int -> (Int, Int) -> Int
countAdjacentMines board m n (x, y) = length $ S.filter (isMine board) adjacents
  where adjacents = getAdjacentSquares m n (x, y)


-- Return whether or not the square at the given coordinates is a mine.
isMine :: [String] -> (Int, Int) -> Bool
isMine [] (x, y) = False
isMine board (x, y) = if (board !! x) !! y == '*' then True else False


-- Get the set of all adjacent squares for a pair of coordinates given the
-- board size.
getAdjacentSquares :: Int -> Int -> (Int, Int) -> Set(Int, Int)
getAdjacentSquares m n (x, y) = S.filter (isInBounds m n) candidates
  where candidates = S.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]


-- Check if a pair of coordinates is in-bounds given the upper bounds m, n for
-- x, y, respectively.
isInBounds :: Int -> Int -> (Int, Int) -> Bool
isInBounds m n (x, y)
  | 0 <= x && x < m && 0 <= y && y < n = True
  | otherwise = False
