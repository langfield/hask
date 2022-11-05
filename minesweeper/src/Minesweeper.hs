module Minesweeper where

import Data.Char (intToDigit)
import Data.Array (Array, (!))
import qualified Data.Ix as Ix
import qualified Data.Array as A

-- Convert a minesweeper board in `[String]` format to an array.
fromList :: (Int, Int) -> [String] -> Array (Int, Int) Char
fromList (m, n) rows = A.listArray ((0, 0), (m - 1, n - 1)) $ concat rows

-- Annotate a minesweeper board.
annotate :: [String] -> [String]
annotate [] = []
annotate rows
  | n == 0 = rows
  | otherwise = map (annotateRow board n) [0 .. m - 1]
  where
    m = length rows
    n = if m > 0 then length $ head rows else 0
    board = fromList (m, n) rows

-- Fill in mine counts for a row of the board.
annotateRow :: Array (Int, Int) Char -> Int -> Int -> String
annotateRow board n x = map (annotateSquare board x) [0 .. n - 1]

-- Fill in mine counts for a single square.
annotateSquare :: Array (Int, Int) Char -> Int -> Int -> Char
annotateSquare board x y
  | isMine board (x, y) = '*'
  | otherwise = if digit == '0' then ' ' else digit
  where
    digit = intToDigit $ countAdjacentMines board (x, y)

-- Count mines adjacent to (x, y) given board size.
countAdjacentMines :: Array (Int, Int) Char -> (Int, Int) -> Int
countAdjacentMines board (x, y) = length $ filter (isMine board) adjacents
  where
    indices = Ix.range ((x - 1, y - 1), (x + 1, y + 1))
    adjacents = filter (Ix.inRange (A.bounds board)) indices

-- Return whether or not the square at the given coordinates is a mine.
isMine :: Array (Int, Int) Char -> (Int, Int) -> Bool
isMine board (x, y) = not (null (A.indices board)) && (board ! (x, y) == '*')
