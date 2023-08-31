module Alphametics (solve) where

import Control.Arrow (second)
import qualified Data.Char as C
import qualified Data.List as L

data Op = Plus | Times | Power
  deriving Show

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

type Combination = [(Char, Int)]

calculateExpression :: Op -> Int -> Int -> Int
calculateExpression Plus  v1 v2 = v1 + v2
calculateExpression Times v1 v2 = v1 * v2
calculateExpression Power v1 v2 = v1 ^ v2

calculateTree :: NumericalExpr -> Int
calculateTree (Atom n) = n
calculateTree (NumericalInfix o t1 t2) =
  calculateExpression o (calculateTree t1) (calculateTree t2)

equationMatches :: NumericalEquation -> Bool
equationMatches (NumericalEquation t n) = calculateTree t == n

convertToValueEquation :: SymbolicEquation
                       -> Combination
                       -> Maybe NumericalEquation
convertToValueEquation e c = convert e
  where
    convert (SymbolicEquation t s) = do
      n  <- numberForString s c
      vt <- convertToValueTree t c
      return (NumericalEquation vt n)

convertToValueTree :: SymbolicExpr -> Combination -> Maybe NumericalExpr
convertToValueTree t c = convert t
  where
    convert :: SymbolicExpr -> Maybe NumericalExpr
    convert (Symbol s              ) = fmap Atom (numberForString s c)
    convert (SymbolicInfix o t1 t2) = do
      v1 <- convertToValueTree t1 c
      v2 <- convertToValueTree t2 c
      return (NumericalInfix o v1 v2)

numberForString :: String -> Combination -> Maybe Int
numberForString s c = digitsToNumber
  <$> noNumberStartingWith0 (digitsForString s c)
  where
    noNumberStartingWith0 (0 : _) = Nothing
    noNumberStartingWith0 xs      = Just xs

digitsForString :: String -> Combination -> [Int]
digitsForString s c = map digitForChar s
  where
    digitForChar i = case lookup i c of
      Just v  -> v
      Nothing -> if C.isNumber i
        then C.digitToInt i
        else error ("char " ++ show i ++ " not found in " ++ show c)

digitsToNumber :: [Int] -> Int
digitsToNumber = foldr (\x s -> x + s * 10) 0 . reverse

generateCombinations :: Int -> [[Int]]
generateCombinations n = go n [0 .. 9] []
  where
    go :: Int -> [Int] -> [Int] -> [[Int]]
    go k xs c
      | k == 0    = [c]
      | otherwise = concatMap (\x -> go (k - 1) (L.delete x xs) (x : c)) xs

testCombination :: SymbolicEquation -> Combination -> Bool
testCombination e c = maybe False equationMatches (convertToValueEquation e c)

testCombinations :: SymbolicEquation -> [Char] -> Maybe Combination
testCombinations e ls = L.find (testCombination e) combinations
  where
    combinations = map combination $ generateCombinations (length ls)
    combination  = zip ls

-- TODO: Memorize this function.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c xs = case dropWhile (== c) xs of
  []  -> []
  xs' -> prefix : splitOn c suffix where (prefix, suffix) = break (== c) xs'

-- | This would be better replaced by something which does the opposite of intercalate.
splitOp :: String -> [String] -> Maybe ([String], [String])
splitOp op xs = case break (== op) xs of
  (_ , []) -> Nothing
  (hs, ts) -> Just (hs, tail ts)

parseTreeRec :: [String] -> [String] -> SymbolicExpr
parseTreeRec ops xs
  | length xs == 1 = Symbol (head xs)
  | otherwise = case splitOp currentOp xs of
    Just (hs, ts) ->
      SymbolicInfix (toOp currentOp) (parseTreeRec ops hs) (parseTreeRec ops ts)
    _ -> parseTreeRec (tail ops) xs
  where currentOp = head ops

parseTree :: [String] -> SymbolicExpr
parseTree = parseTreeRec ["+", "*", "^"]

parse :: String -> Maybe SymbolicEquation
parse s = Just (SymbolicEquation (parseTree lhs) rhs)
  where (lhs, rhs) = (second last . break (== "==") . words) s

toOp :: String -> Op
toOp "+" = Plus
toOp "*" = Times
toOp "^" = Power
toOp _   = error "Bad operator"

letters :: String -> [Char]
letters = L.nub . filter C.isUpper

solve :: String -> Maybe Combination
solve puzzle = do
  e <- parse puzzle
  testCombinations e (letters puzzle)
