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
data NumericExpr = Atom Int | NumericInfix Op NumericExpr NumericExpr
  deriving Show

-- LHS (an expression) and RHS (an integer).
data NumericEquation = NumericEquation NumericExpr Int
  deriving Show

type LetterMap = [(Char, Int)]

eval :: NumericExpr -> Int
eval (Atom n                ) = n
eval (NumericInfix Plus  x y) = eval x + eval y
eval (NumericInfix Times x y) = eval x * eval y
eval (NumericInfix Power x y) = eval x ^ eval y

equationMatches :: NumericEquation -> Bool
equationMatches (NumericEquation lhs rhs) = eval lhs == rhs

substituteEqn :: SymbolicEquation -> LetterMap -> Maybe NumericEquation
substituteEqn (SymbolicEquation lhs rhs) mapping =
  NumericEquation <$> substituteExpr lhs mapping <*> encodeWord mapping rhs

substituteExpr :: SymbolicExpr -> LetterMap -> Maybe NumericExpr
substituteExpr (Symbol s) mapping = Atom <$> encodeWord mapping s
substituteExpr (SymbolicInfix op x y) mapping =
  NumericInfix op <$> substituteExpr x mapping <*> substituteExpr y mapping

toDigit :: LetterMap -> Char -> Maybe Int
toDigit mapping c
  | C.isNumber c = Just (C.digitToInt c)
  | otherwise    = lookup c mapping

toDigits :: LetterMap -> [Char] -> Maybe [Int]
toDigits mapping = mapM (toDigit mapping)

validateDigits :: [Int] -> Maybe [Int]
validateDigits (0 : _) = Nothing
validateDigits xs      = Just xs

intify :: [Int] -> Int
intify = foldr (\x acc -> x + acc * 10) 0 . reverse

-- 1. Map letters to digits ([Char] -> [Int])
-- 2. Validate, i.e. make sure no leading zeroes ([Int] -> Maybe [Int])
-- 3. Reduce to a single Int (Maybe [Int] -> Maybe Int)
encodeWord :: LetterMap -> String -> Maybe Int
encodeWord mapping s = intify <$> (validateDigits =<< toDigits mapping s)

-- | Generate combinations of length n of digits [0..9].
generateCombinations :: Int -> [[Int]]
generateCombinations n = go n [0 .. 9] []
  where
    go :: Int -> [Int] -> [Int] -> [[Int]]
    go k xs c
      | k == 0    = [c]
      | otherwise = concatMap (\x -> go (k - 1) (L.delete x xs) (x : c)) xs

isSolution :: SymbolicEquation -> LetterMap -> Bool
isSolution eqn = maybe False equationMatches . substituteEqn eqn

findSolution :: [Char] -> SymbolicEquation -> Maybe LetterMap
findSolution letters eqn =
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
solve puzzle =
  findSolution (L.nub . filter C.isUpper $ puzzle) =<< parse puzzle
