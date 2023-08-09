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

rights :: Eq a => a -> [a] -> [(a, Maybe a)]
rights _ [] = []
rights _ [x] = [(x, Nothing)]
rights c (x : y : rest)
  | y /= c = (x, Just y) : rights c (y : rest)
  | otherwise = (x, Nothing) : rights c (y : rest)

lefts :: Eq a => a -> [a] -> [(a, Maybe a)]
lefts c = reverse . rights c . reverse

merge :: (a, Maybe a) -> (a, Maybe a) -> (Maybe a, a, Maybe a)
merge (x, l) (_, r) = (l, x, r)

neighbors :: Eq a => a -> [a] -> [(Maybe a, a, Maybe a)]
neighbors c xs = zipWith merge (lefts c xs) (rights c xs)

cmp :: Ord a => (Maybe a, a, Maybe a) -> (Maybe a, a, Maybe a) -> Ordering
cmp (_, x, _) (_, y, _) = compare x y

merge2D :: (Maybe a, a, Maybe a) -> (Maybe a, a, Maybe a) -> (a, [a])
merge2D (l, x, r) (a, _, b) = (x, MB.catMaybes [l, r, a, b])

neighbors2D :: Ord a => a -> [[a]] -> Map a [a]
neighbors2D c xss = M.fromList $ zipWith merge2D horizontals verticals
  where
    horizontals = concatMap (neighbors c) xss
    verticals = L.sortBy cmp . concatMap (neighbors c) . L.transpose $ xss

stagger :: Ord a => a -> [[a]] -> [[a]]
stagger = stagger' 0

stagger' :: Ord a => Int -> a -> [[a]] -> [[a]]
stagger' _ _ [] = []
stagger' n c (xs : xss)
  | n <= 0 = xs : stagger' 1 c xss
  | otherwise = (prefix ++ xs) : stagger' (n + 1) c xss
  where
    prefix = replicate n c

diags :: Ord a => a -> [[a]] -> [(Maybe a, a, Maybe a)]
diags c = concatMap (neighbors c) . stagger c
