module Connect (Mark(..), winner, hexagons) where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Maybe as MB
import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace (trace)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

data Mark = Cross | Nought deriving (Eq, Show)
type Board = [String]
data Player = X | O deriving (Eq, Ord, Show)

data Hex = Null | Empty Int Int | XO Player Int Int
  deriving (Eq, Ord)

instance Show Hex where
  show Null = "Null"
  show (Empty x y) = "Empty(" ++ show x ++ ", " ++ show y ++ ")"
  show (XO p x y) = show p ++ "(" ++ show x ++ ", " ++ show y ++ ")"

coords :: Hex -> (Int, Int)
coords (Empty x y) = (x, y)
coords (XO _ x y ) = (x, y)
coords Null        = (-1, -1)

winner :: [String] -> Maybe Mark
winner board
  | cross     = Just Cross
  | nought    = Just Nought
  | otherwise = Nothing
  where
    board' = map (filter (/= ' ')) board
    nought = won O board'
    cross  = won X (L.transpose board')

-- | Check if player `c` has connected top-to-bottom.
--
-- The easiest way to do this is just to return `True` when we've reached the
-- last row. This should probably be a simple DFS function, right?
won :: Player -> Board -> Bool
won _ []    = False
won c board = or outcomes
  where
    hexs     = trace' "hexs" $ mkhexs $ trace' "board" board
    graph    = trace' "graph" $ hexagons Null hexs
    targets  = S.fromList . map coords . last $ hexs
    outcomes = [ search c graph targets start S.empty | start <- head hexs ]

isPlayer :: Player -> Hex -> Bool
isPlayer _  Null        = False
isPlayer _  (Empty _ _) = False
isPlayer p' (XO p _ _ ) = p == p'

search :: Player -> Map Hex [Hex] -> Set (Int, Int) -> Hex -> Set Hex -> Bool
search _ _ _ (Empty _ _) _ = False
search _ _ _ Null        _ = False
search p' graph targets start@(XO p x y) visiting
  | p /= p' = False
  | (x, y) `S.member` targets = True
  | otherwise = case nbs of
    Just nbs' -> or [ search p' graph targets nb (S.insert nb visiting) | nb <- nbs' ]
    Nothing   -> False
  where nbs = filter (isPlayer p') . filter (`S.member` visiting) <$> M.lookup start graph

-- We essentially want to iterate over something that gives us a 7-tuple, where
-- we get the current element, and its 6 neighbors. And then we can put them
-- all in a data structure.
--
-- One good way to start might be to solve the 1-dimensional case first, where
-- we have a list and we want to get 3-tuples of each element and its
-- neighbors.

rights :: Eq a => a -> [a] -> [(a, Maybe a)]
rights _ []  = []
rights _ [x] = [(x, Nothing)]
rights c (x : y : rest)
  | y /= c    = (x, Just y) : rights c (y : rest)
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
    verticals   = L.sortBy cmp . concatMap (neighbors c) . L.transpose $ xss

stagger :: Ord a => a -> [[a]] -> [[a]]
stagger = stagger' 0

stagger' :: Ord a => Int -> a -> [[a]] -> [[a]]
stagger' _ _ [] = []
stagger' n c (xs : xss)
  | n <= 0    = xs : stagger' 1 c xss
  | otherwise = (prefix ++ xs) : stagger' (n + 1) c xss
  where prefix = replicate n c

diagonals :: Ord a => a -> [[a]] -> [(Maybe a, a, Maybe a)]
diagonals c = concatMap (neighbors c) . L.transpose . stagger c

listify :: (Maybe a, a, Maybe a) -> (a, [a])
listify (Just l , x, Just r ) = (x, [l, r])
listify (Just l , x, Nothing) = (x, [l])
listify (Nothing, x, Just r ) = (x, [r])
listify (Nothing, x, Nothing) = (x, [])

-- | Map with index of iteration.
map' :: (Int -> a -> b) -> [a] -> [b]
map' = go 0
  where
    go :: Int -> (Int -> a -> b) -> [a] -> [b]
    go _ _ []       = []
    go i f (x : xs) = f i x : go (i + 1) f xs

mkhex :: Int -> Int -> Char -> Hex
mkhex i j 'X' = XO X i j
mkhex i j 'O' = XO O i j
mkhex i j _   = Empty i j

mkhexs :: [[Char]] -> [[Hex]]
mkhexs = map' go
  where
    go :: Int -> [Char] -> [Hex]
    go i = map' (mkhex i)

hexagons :: Ord a => a -> [[a]] -> Map a [a]
hexagons c xss = M.delete c . M.unionWith (++) (neighbors2D c xss) . M.fromList . map listify . diagonals c $ xss
