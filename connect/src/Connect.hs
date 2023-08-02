module Connect (Mark(..), winner) where

import Data.Tuple (swap)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

data Mark = Cross | Nought deriving (Eq, Show)
type Board = [String]
type Player = Char

data Node = Empty | Node Int [Node]

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
build :: [[Char]] -> Node
build [] = Empty
build ([] : _) = Empty
build ((x : xs) : xss) = Empty

build' :: [Char] -> Node
build' [] = Empty

rights :: [a] -> [(a, a)]
rights (x : y : rest) = (x, y) : rights (y : rest)
rights _ = []

lefts :: [a] -> [(a, a)]
lefts = map swap . rights

rights' :: [a] -> [(a, Maybe a)]
rights' [] = []
rights' [x] = [(x, Nothing)]
rights' (x : y : rest) = (x, Just y) : rights' (y : rest)

lefts' :: [a] -> [(a, Maybe a)]
lefts' = reverse . rights' . reverse

merge :: (a, Maybe a) -> (a, Maybe a) -> (Maybe a, a, Maybe a)
merge (x, l) (_, r) = (l, x, r)

neighbors :: [a] -> [(Maybe a, a, Maybe a)]
neighbors xs = zipWith merge (lefts' xs) (rights' xs)

rights'' :: Ord a => [a] -> Map a (Maybe a)
rights'' [] = M.empty
rights'' [x] = M.singleton x Nothing
rights'' (x : y : rest) = M.insert x (Just y) (rights'' (y : rest))

lefts'' :: Ord a => [a] -> Map a (Maybe a)
lefts'' [] = M.empty
lefts'' [x] = M.singleton x Nothing
lefts'' (x : y : rest) = M.insert x (Just y) (lefts'' (y : rest))
