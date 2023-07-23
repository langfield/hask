module Connect (Mark(..), winner) where

import qualified Data.List as L

data Mark = Cross | Nought deriving (Eq, Show)
type Board = [String]
type Player = Char

winner :: [String] -> Maybe Mark
winner board
  | cross = Just Cross
  | nought = Just Nought
  | otherwise = Nothing
  where
    cross = won 'X' board
    nought = won 'O' (L.transpose board)

-- | Check if `c` has connected top-to-bottom.
--
-- The easiest way to do this is just to return `True` when we've reached the
-- last row. This should probably be a simple DFS function, right?
won :: Player -> Board -> Bool
won _ [] = False
won c board@(row : _) = or outcomes
  where
    m = length board
    n = length row
    outcomes = [search c board (i, j) | i <- [0..m], j <- [0..n]]

search :: Player -> Board -> Int -> (Int, Int) -> Bool
search _ [] _ _ = True
search c rows m (i, j)
  | i == m - 1 = True
  | otherwise = False
  where
    c' = rows[i][j]
