module Alphametics (solve) where

import Control.Arrow (second)
import qualified Data.Char as C
import qualified Data.List as L

data Op = Plus | Times | Power

instance Show Op where
  show Plus  = "+"
  show Times = "*"
  show Power = "^"

-- Symbolic atoms and infix binary operators.
data SymbolicExpr = Symbol String | SymbolicInfix Op SymbolicExpr SymbolicExpr
  deriving Show

-- LHS (an expression) and RHS (just a single token/symbol).
data SymbolicEquation = SymbolicEquation SymbolicExpr String
  deriving Show

-- Integral atoms and infix binary operators.
data NumericalExpr = Atom Int | NumericalInfix Op NumericalExpr NumericalExpr
  deriving Show

-- LHS (an expression) and RHS (an integer).
data NumericalEquation = NumericalEquation NumericalExpr Int
  deriving Show

type LetterMap = [(Char, Int)]

eval :: NumericalExpr -> Int
eval (Atom n                  ) = n
eval (NumericalInfix Plus  x y) = eval x + eval y
eval (NumericalInfix Times x y) = eval x * eval y
eval (NumericalInfix Power x y) = eval x ^ eval y

equationMatches :: NumericalEquation -> Bool
equationMatches (NumericalEquation lhs rhs) = eval lhs == rhs

substituteEqn :: SymbolicEquation -> LetterMap -> Maybe NumericalEquation
substituteEqn (SymbolicEquation lhs rhs) mapping =
  NumericalEquation <$> substituteExpr lhs mapping <*> encodeWord mapping rhs

substituteExpr :: SymbolicExpr -> LetterMap -> Maybe NumericalExpr
substituteExpr (Symbol s) mapping = Atom <$> encodeWord mapping s
substituteExpr (SymbolicInfix op x y) mapping =
  NumericalInfix op <$> substituteExpr x mapping <*> substituteExpr y mapping

-- 1. Map letters to digits ([Char] -> [Int])
-- 2. Validate, i.e. make sure no leading zeroes ([Int] -> Maybe [Int])
-- 3. Reduce to a single Int (Maybe [Int] -> Maybe Int)
encodeWord :: LetterMap -> String -> Maybe Int
encodeWord mapping s = intify <$> (validateDigits =<< toDigits s)
  where
    toDigits :: [Char] -> Maybe [Int]
    toDigits = mapM (toDigit mapping)

    validateDigits :: [Int] -> Maybe [Int]
    validateDigits (0 : _) = Nothing
    validateDigits xs      = Just xs

    intify :: [Int] -> Int
    intify = foldr (\x acc -> x + acc * 10) 0 . reverse

toDigit :: LetterMap -> Char -> Maybe Int
toDigit mapping c
  | C.isNumber c = Just (C.digitToInt c)
  | otherwise    = lookup c mapping

isSolution :: SymbolicEquation -> LetterMap -> Bool
isSolution eqn = maybe False equationMatches . substituteEqn eqn

-- | Generate combinations of length n of digits [0..9].
generateCombinations :: Int -> [[Int]]
generateCombinations n = go n [0 .. 9] []
  where
    go :: Int -> [Int] -> [Int] -> [[Int]]
    go k xs c
      | k == 0    = [c]
      | otherwise = concatMap (\x -> go (k - 1) (L.delete x xs) (x : c)) xs

findSolution :: SymbolicEquation -> [Char] -> Maybe LetterMap
findSolution eqn letters =
  L.find (isSolution eqn)
    . map (zip letters)
    . generateCombinations
    . length
    $ letters

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c xs = case dropWhile (== c) xs of
  []  -> []
  xs' -> prefix : splitOn c suffix where (prefix, suffix) = break (== c) xs'

-- | Given a list of infix operations in reverse order of precedence and a list
-- of space-delimited tokens, build a symbolic expression.
parseTree :: [Op] -> [String] -> Maybe SymbolicExpr
parseTree [] [x] = Just (Symbol x)
parseTree [] _   = Nothing
parseTree (op : ops) xs =
  treeify op =<< (mapM (parseTree ops) . splitOn (show op) $ xs)

treeify :: Op -> [SymbolicExpr] -> Maybe SymbolicExpr
treeify _  []       = Nothing
treeify _  [x     ] = Just x
treeify op (x : xs) = SymbolicInfix op x <$> treeify op xs

parse :: String -> Maybe SymbolicEquation
parse s = SymbolicEquation <$> parseTree [Plus, Times, Power] lhs <*> Just rhs
  where (lhs, rhs) = (second last . break (== "==") . words) s

solve :: String -> Maybe LetterMap
solve puzzle = do
  eqn <- parse puzzle
  findSolution eqn . L.nub . filter C.isUpper $ puzzle
