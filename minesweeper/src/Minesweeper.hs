module Minesweeper annotate where

import Data.Array as A
import Data.Array (Array, listArray)
import Data.Char (intToDigit)
import Data.Map (Map, fromListWith)
import Data.Set (Set)
import Debug.Trace (trace)

import qualified Data.Ix as Ix
import qualified Data.Map as M
import qualified Data.Set as S

traceAs :: Show a => String -> a -> a
traceAs s x = trace (s ++ ": " ++ show x) x

fromList :: Int -> Int -> [String] -> Array Int (Array Int Char)
fromList m n ss = listArray (0, m - 1) $ [listArray (0, n - 1) (ss !! i) | i <- [0 .. m - 1]]

-- Annotate a minesweeper board.
annotate :: [String] -> [String]
annotate [] = []
annotate ss
  | n == 0 = ss
  | otherwise = map (annotateRow board n adjacentMineCountMap) [0 .. m - 1]
  where m = length ss n = if m > 0 then length $ head ss else 0
        board = fromList m n ss
        squares = [(x, y) | x <- [0 .. m - 1], y <- [0 .. n - 1]]
        adjacentMineCounts = map (\s -> (s, countAdjacentMines board m n s)) squares
        adjacentMineCountMap = fromListWith (+) adjacentMineCounts

-- Fill in mine counts for a row of the board.
annotateRow :: Array Int (Array Int Char) -> Int -> Map (Int, Int) Int -> Int -> String
annotateRow board n adjacentMineCountMap x = map (annotateSquare board adjacentMineCountMap x) [0 .. n - 1]

-- Fill in mine counts for a single square.
annotateSquare :: Array Int (Array Int Char) -> Map (Int, Int) Int -> Int -> Int -> Char
annotateSquare board adjacentMineCountMap x y
  | isMine board (x, y) = '*'
  | otherwise = if digit == '0' then ' ' else digit
  where digit = intToDigit $ adjacentMineCountMap M.! (x, y)

-- Count mines adjacent to (x, y) given board size.
countAdjacentMines :: Array Int (Array Int Char) -> Int -> Int -> (Int, Int) -> Int
countAdjacentMines board m n (x, y) = length $ S.filter (isMine board) adjacents
  where adjacents = getAdjacentSquares m n (x, y)

-- Return whether or not the square at the given coordinates is a mine.
isMine :: Array Int (Array Int Char) -> (Int, Int) -> Bool
isMine board (x, y) = size > 0 && (board A.! x A.! y == '*')
  where size = Ix.rangeSize . bounds $ board

-- Get the set of all adjacent squares for a pair of coordinates given the
-- board size.
getAdjacentSquares :: Int -> Int -> (Int, Int) -> Set (Int, Int)
getAdjacentSquares m n (x, y) = S.filter (isInBounds m n) candidates
  where candidates =
      S.fromList
        [ (x - 1, y)
        , (x + 1, y)
        , (x, y - 1)
        , (x, y + 1)
        , (x - 1, y - 1)
        , (x - 1, y + 1)
        , (x + 1, y - 1)
        , (x + 1, y + 1)
        ]

-- Check if a pair of coordinates is in-bounds given the upper bounds m, n for
-- x, y, respectively.
isInBounds :: Int -> Int -> (Int, Int) -> Bool
isInBounds m n (x, y)
  | 0 <= x && x < m && 0 <= y && y < n = True
  | otherwise = False
