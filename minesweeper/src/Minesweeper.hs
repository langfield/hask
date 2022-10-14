module Minesweeper where

import Data.Array as A
import Data.Char (intToDigit)

import qualified Data.Ix as Ix

-- Convert a minesweeper board in `[String]` format to an array.
fromList :: (Int, Int) -> [String] -> Array (Int, Int) Char
fromList (m, n) rows = listArray ((0, 0), (m - 1, n - 1)) $ [rows !! i !! j | i <- [0 .. m - 1], j <- [0 .. n - 1]]

-- Annotate a minesweeper board.
annotate :: [String] -> [String]
annotate [] = []
annotate rows
  | n == 0 = rows
  | otherwise = map (annotateRow board (m, n)) [0 .. m - 1]
  where m = length rows
        n = if m > 0 then length $ head rows else 0
        board = fromList (m, n) rows

-- Fill in mine counts for a row of the board.
annotateRow :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
annotateRow board (m, n) x = map (annotateSquare board (m, n) x) [0 .. n - 1]

-- Fill in mine counts for a single square.
annotateSquare :: Array (Int, Int) Char -> (Int, Int) -> Int -> Int -> Char
annotateSquare board (m, n) x y
  | isMine board (x, y) = '*'
  | otherwise = if digit == '0' then ' ' else digit
  where digit = intToDigit $ countAdjacentMines board (m, n) (x, y)

-- Count mines adjacent to (x, y) given board size.
countAdjacentMines :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Int
countAdjacentMines board (m, n) (x, y) = length $ filter (isMine board) adjacents
  where adjacents = filter (inRange ((0, 0), (m - 1, n - 1))) $ range ((x - 1, y - 1), (x + 1, y + 1))

-- Return whether or not the square at the given coordinates is a mine.
isMine :: Array (Int, Int) Char -> (Int, Int) -> Bool
isMine board (x, y) = size > 0 && (board A.! (x, y) == '*')
  where size = Ix.rangeSize . bounds $ board
