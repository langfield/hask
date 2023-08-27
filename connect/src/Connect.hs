module Connect (Mark(..), winner, hexagons) where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Maybe as MB
import Data.Set (Set)
import qualified Data.Set as S

-- We map each element in the 2D array to a list of its 6 neighbors.

data Mark = Cross | Nought deriving (Eq, Show)
type Board = [String]
data Player = X | O deriving (Eq, Ord, Show)

data Hex = Null | Empty Int Int | XO Player Int Int
  deriving (Eq, Ord)

instance Show Hex where
  show Null        = "Null"
  show (Empty x y) = "Empty(" ++ show x ++ ", " ++ show y ++ ")"
  show (XO p x y ) = show p ++ "(" ++ show x ++ ", " ++ show y ++ ")"

isPlayer :: Player -> Hex -> Bool
isPlayer _  Null        = False
isPlayer _  (Empty _ _) = False
isPlayer p' (XO p _ _ ) = p == p'

-- | Right neighbors in a list.
rights :: Ord a => a -> [a] -> Map a [a]
rights _ []  = M.empty
rights _ [x] = M.singleton x []
rights c (x : y : rest)
  | y /= c    = M.insert x [y] (rights c (y : rest))
  | otherwise = M.insert x [] (rights c (y : rest))

-- | Left neighbors in a list.
lefts :: Ord a => a -> [a] -> Map a [a]
lefts c = rights c . reverse

-- | Left and right neighbors in a list.
neighbors :: Ord a => a -> [a] -> Map a [a]
neighbors c xs = M.unionWith (++) (lefts c xs) (rights c xs)

-- | 4-way neighbors in a 2D grid.
neighbors2D :: Ord a => a -> [[a]] -> Map a [a]
neighbors2D c xss = M.unionWith (++) horizontals verticals
  where
    horizontals = foldr (M.unionWith (++) . neighbors c) M.empty xss
    verticals =
      foldr (M.unionWith (++) . neighbors c) M.empty . L.transpose $ xss

stagger' :: Ord a => Int -> a -> [[a]] -> [[a]]
stagger' _ _ [] = []
stagger' n c (xs : xss)
  | n <= 0    = xs : stagger' 1 c xss
  | otherwise = (prefix ++ xs) : stagger' (n + 1) c xss
  where prefix = replicate n c

stagger :: Ord a => a -> [[a]] -> [[a]]
stagger = stagger' 0

-- | Top-right and bottom-left neighbors in a 2D grid.
diagonals :: Ord a => a -> [[a]] -> Map a [a]
diagonals c =
  foldr (M.unionWith (++) . neighbors c) M.empty . L.transpose . stagger c

-- | Map with index of iteration.
map' :: (Int -> a -> b) -> [a] -> [b]
map' = go 0
  where
    go :: Int -> (Int -> a -> b) -> [a] -> [b]
    go _ _ []       = []
    go i f (x : xs) = f i x : go (i + 1) f xs

-- | Construct a Hex from coordinates.
mkhex :: Int -> Int -> Char -> Hex
mkhex i j 'X' = XO X i j
mkhex i j 'O' = XO O i j
mkhex i j _   = Empty i j

-- | Construct 2D grid of Hexs from a board.
mkhexs :: [[Char]] -> [[Hex]]
mkhexs = map' go
  where
    go :: Int -> [Char] -> [Hex]
    go i = map' (mkhex i)

-- | Construct graph of hexagonal neighbors from a 2D grid of Hexs.
--
-- Note that `c` is used to left-pad rows in `stagger`.
hexagons :: Hex -> [[Hex]] -> Map Hex [Hex]
hexagons c xss =
  M.delete c . M.unionWith (++) (neighbors2D c xss) . diagonals c $ xss

-- | Check if a target is reachable from a start Hex as a given player.
search :: Player -> Map Hex [Hex] -> Set Hex -> Hex -> Set Hex -> Bool
search _ _ _ Null        _ = False
search _ _ _ (Empty _ _) _ = False
search p' graph targets start@(XO p _ _) visiting
  | p /= p' = False
  | start `S.member` targets = True
  | otherwise = or
    [ search p' graph targets nb (S.insert nb visiting) | nb <- nbs' ]
  where
    nbs  = MB.fromMaybe [] (M.lookup start graph)
    nbs' = filter (isPlayer p') . filter (`S.notMember` visiting) $ nbs

-- | Check if player `c` has connected top-to-bottom.
won :: Player -> Board -> Bool
won c board = or outcomes
  where
    hexs     = mkhexs board
    graph    = hexagons Null hexs
    starts   = filter (isPlayer c) . head $ hexs
    targets  = S.fromList . filter (isPlayer c) . last $ hexs
    outcomes = [ search c graph targets start S.empty | start <- starts ]

-- | Compute the Mark of the winner of a given board.
winner :: [String] -> Maybe Mark
winner board
  | won X (L.transpose board') = Just Cross
  | won O board'               = Just Nought
  | otherwise                  = Nothing
  where board' = map (filter (/= ' ')) board
