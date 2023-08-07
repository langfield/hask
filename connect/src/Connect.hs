module Connect (Mark(..), winner, neighbors2D) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as MB

data Mark = Cross | Nought deriving (Eq, Show)
type Board = [String]
type Player = Char

data Node a = Empty | Node a [Node a]

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
    outcomes = [search c board m (i, j) | i <- [0..m], j <- [0..n]]

search :: Player -> Board -> Int -> (Int, Int) -> Bool
search _ [] _ _ = True
search c rows m (i, j)
  | i == m - 1 = True
  | otherwise = False

-- We essentially want to iterate over something that gives us a 7-tuple, where
-- we get the current element, and its 6 neighbors. And then we can put them
-- all in a data structure.
--
-- One good way to start might be to solve the 1-dimensional case first, where
-- we have a list and we want to get 3-tuples of each element and its
-- neighbors.

rights :: [a] -> [(a, Maybe a)]
rights [] = []
rights [x] = [(x, Nothing)]
rights (x : y : rest) = (x, Just y) : rights (y : rest)

lefts :: [a] -> [(a, Maybe a)]
lefts = reverse . rights . reverse

merge :: (a, Maybe a) -> (a, Maybe a) -> (Maybe a, a, Maybe a)
merge (x, l) (_, r) = (l, x, r)

neighbors :: [a] -> [(Maybe a, a, Maybe a)]
neighbors xs = zipWith merge (lefts xs) (rights xs)

cmp :: Ord a => (Maybe a, a, Maybe a) -> (Maybe a, a, Maybe a) -> Ordering
cmp (_, x, _) (_, y, _) = compare x y

merge2D :: (Maybe a, a, Maybe a) -> (Maybe a, a, Maybe a) -> (a, [a])
merge2D (l, x, r) (a, _, b) = (x, MB.catMaybes [l, r, a, b])

neighbors2D :: Ord a => [[a]] -> Map a [a]
neighbors2D xss = M.fromList $ zipWith merge2D horizontals verticals
  where
    horizontals = concatMap neighbors xss
    verticals = L.sortBy cmp . concatMap neighbors . L.transpose $ xss
