module Connect (Mark(..), winner, hexagons) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Map as M
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
  show Null        = "Null"
  show (Empty x y) = "Empty(" ++ show x ++ ", " ++ show y ++ ")"
  show (XO p x y ) = show p ++ "(" ++ show x ++ ", " ++ show y ++ ")"

winner :: [String] -> Maybe Mark
winner board
  | cross     = Just Cross
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
    hexs    = mkhexs $ trace' "board" board
    graph   = hexagons Null hexs
    starts  = filter (isPlayer c) . head $ hexs
    targets = S.fromList . trace' "targets" . filter (isPlayer c) . last $ hexs
    outcomes =
      trace' "outcomes"
        $ [ search (trace' "searching for" c) graph targets start S.empty
          | start <- starts
          ]

isPlayer :: Player -> Hex -> Bool
isPlayer _  Null        = False
isPlayer _  (Empty _ _) = False
isPlayer p' (XO p _ _ ) = p == p'

search :: Player -> Map Hex [Hex] -> Set Hex -> Hex -> Set Hex -> Bool
search _ _ _ (Empty _ _) _ = False
search _ _ _ Null        _ = False
search p' graph targets start@(XO p _ _) visiting
  | p /= p' = False
  | start `S.member` targets = seq (trace' "found target" start) True
  | otherwise = case seq (trace' "node and its neighbors" (start, nbs)) nbs of
    Just nbs' ->
      or [ search p' graph targets nb (S.insert nb visiting) | nb <- nbs' ]
    Nothing -> False
  where
    nbs =
      filter (isPlayer p')
        .   filter (`S.notMember` visiting)
        <$> M.lookup start graph

-- We essentially want to iterate over something that gives us a 7-tuple, where
-- we get the current element, and its 6 neighbors. And then we can put them
-- all in a data structure.
--
-- One good way to start might be to solve the 1-dimensional case first, where
-- we have a list and we want to get 3-tuples of each element and its
-- neighbors.

rights :: Ord a => a -> [a] -> Map a [a]
rights _ []  = M.empty
rights _ [x] = M.singleton x []
rights c (x : y : rest)
  | y /= c    = M.insert x [y] (rights c (y : rest))
  | otherwise = M.insert x [] (rights c (y : rest))

lefts :: Ord a => a -> [a] -> Map a [a]
lefts c = rights c . reverse

neighbors :: Ord a => a -> [a] -> Map a [a]
neighbors c xs = M.unionWith (++) (lefts c xs) (rights c xs)

neighbors2D :: Ord a => a -> [[a]] -> Map a [a]
neighbors2D c xss = M.unionWith (++) horizontals verticals
  where
    horizontals = foldr (M.unionWith (++) . neighbors c) M.empty xss
    verticals   = foldr (M.unionWith (++) . neighbors c) M.empty . L.transpose $ xss

stagger :: Ord a => a -> [[a]] -> [[a]]
stagger = stagger' 0

stagger' :: Ord a => Int -> a -> [[a]] -> [[a]]
stagger' _ _ [] = []
stagger' n c (xs : xss)
  | n <= 0    = xs : stagger' 1 c xss
  | otherwise = (prefix ++ xs) : stagger' (n + 1) c xss
  where prefix = replicate n c

diagonals :: Ord a => a -> [[a]] -> Map a [a]
diagonals c = foldr (M.unionWith (++) . neighbors c) M.empty . L.transpose . stagger c

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

hexagons :: Hex -> [[Hex]] -> Map Hex [Hex]
hexagons c xss =
  M.delete c
    . M.unionWith (++) (trace' "neighbors 2D" $ neighbors2D c xss)
    . diagonals c
    $ xss
