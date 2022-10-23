module Minesweeper where

import Data.Char (intToDigit)
import Data.Array.Comfort.Boxed (Array, (!))

import qualified Data.Array.Comfort.Boxed as A
import qualified Data.Array.Comfort.Shape as Sh


-- Convert a minesweeper board in `[String]` format to an array.
fromList :: (Int, Int) -> [String] -> Array (Sh.ZeroBased Int, Sh.ZeroBased Int) Char
fromList (m, n) rows = A.fromList (Sh.ZeroBased m, Sh.ZeroBased n) $ concat rows

-- Annotate a minesweeper board.
annotate :: [String] -> [String]
annotate [] = []
annotate rows
  | n == 0 = rows
  | otherwise = map (annotateRow board n) [0 .. m - 1]
  where m = length rows
        n = if m > 0 then length $ head rows else 0
        board = fromList (m, n) rows

-- Fill in mine counts for a row of the board.
annotateRow :: Array (Sh.ZeroBased Int, Sh.ZeroBased Int) Char -> Int -> Int -> String
annotateRow board n x = map (annotateSquare board x) [0 .. n - 1]

-- Fill in mine counts for a single square.
annotateSquare :: Array (Sh.ZeroBased Int, Sh.ZeroBased Int) Char -> Int -> Int -> Char
annotateSquare board x y
  | isMine board (x, y) = '*'
  | otherwise = if digit == '0' then ' ' else digit
  where digit = intToDigit $ countAdjacentMines board (x, y)

-- Count mines adjacent to (x, y) given board size.
countAdjacentMines :: Array (Sh.ZeroBased Int, Sh.ZeroBased Int) Char -> (Int, Int) -> Int
countAdjacentMines board (x, y) = length $ filter (isMine board) adjacents
  where indices = Sh.indices $ Sh.Range (x - 1, y - 1) (x + 1, y + 1)
        adjacents = filter (Sh.inBounds (A.shape board)) indices

-- Return whether or not the square at the given coordinates is a mine.
isMine :: Array (Sh.ZeroBased Int, Sh.ZeroBased Int) Char -> (Int, Int) -> Bool
isMine board (x, y) = size > 0 && (board ! (x, y) == '*')
  where size = Sh.size . A.shape $ board
