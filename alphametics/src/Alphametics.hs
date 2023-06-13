module Alphametics (solve) where

import qualified Data.Char as C
import qualified Data.List as L

data Operator = Plus | Times | Power
  deriving (Show)

data TTree = TLeaf String | TTree Operator TTree TTree
  deriving (Show)

data TEquation = TE TTree String
  deriving Show

data VTree = VLeaf Int | VTree Operator VTree VTree
  deriving (Show)

data VEquation = VE VTree Int
  deriving Show

type Combination = [(Char, Int)]

calculateExpression :: Operator -> Int -> Int -> Int
calculateExpression Plus  v1 v2 = v1 + v2
calculateExpression Times v1 v2 = v1 * v2
calculateExpression Power v1 v2 = v1 ^ v2

calculateTree :: VTree -> Int
calculateTree (VLeaf n      ) = n
calculateTree (VTree o t1 t2) = calculateExpression o (calculateTree t1) (calculateTree t2)

equationMatches :: VEquation -> Bool
equationMatches (VE t n) = calculateTree t == n

convertToValueEquation :: TEquation -> Combination -> Maybe VEquation
convertToValueEquation e c = convert e
  where
    convert (TE t s) = do
      n  <- numberForString s c
      vt <- convertToValueTree t c
      return (VE vt n)

convertToValueTree :: TTree -> Combination -> Maybe VTree
convertToValueTree t c = convert t
  where
    convert :: TTree -> Maybe VTree
    convert (TLeaf s      ) = fmap VLeaf (numberForString s c)
    convert (TTree o t1 t2) = do
      v1 <- convertToValueTree t1 c
      v2 <- convertToValueTree t2 c
      return (VTree o v1 v2)

numberForString :: String -> Combination -> Maybe Int
numberForString s c = digitsToNumber <$> noNumberStartingWith0 (digitsForString s c)
  where
    noNumberStartingWith0 (0 : _) = Nothing
    noNumberStartingWith0 xs      = Just xs

digitsForString :: String -> Combination -> [Int]
digitsForString s c = map digitForChar s
  where
    digitForChar i = case lookup i c of
      Just v  -> v
      Nothing -> if C.isNumber i then C.digitToInt i else error ("char " ++ show i ++ " not found in " ++ show c)

digitsToNumber :: [Int] -> Int
digitsToNumber = foldr (\x s -> x + s * 10) 0 . reverse

generateCombinations :: Int -> [[Int]]
generateCombinations l = genRec l [0 .. 9] []
  where
    genRec :: Int -> [Int] -> [Int] -> [[Int]]
    genRec l' xs c
      | l' == 0   = [c]
      | otherwise = concatMap (\x -> genRec (l' - 1) (L.delete x xs) (x : c)) xs

testCombination :: TEquation -> Combination -> Bool
testCombination e c = maybe False equationMatches (convertToValueEquation e c)

testCombinations :: TEquation -> [Char] -> Maybe Combination
testCombinations e ls = L.find (testCombination e) combinations
  where
    combinations = map combination $ generateCombinations (length ls)
    combination  = zip ls

parse :: String -> Maybe TEquation
parse s = Just (TE (parseTree ts) r)
  where
    (ts, r) = (sides . words) s
    sides   = removeEqual . break (== "==")
    removeEqual (hs, us) = (hs, last us)

splitOp :: String -> [String] -> Maybe ([String], [String])
splitOp op xs = case break (== op) xs of
  (_ , []) -> Nothing
  (hs, ts) -> Just (hs, tail ts)

parseTree :: [String] -> TTree
parseTree = parseTreeRec ["+", "*", "^"]

parseTreeRec :: [String] -> [String] -> TTree
parseTreeRec ops xs
  | length xs == 1 = TLeaf (head xs)
  | otherwise = case splitOp currentOp xs of
    Just (hs, ts) -> TTree (toOperator currentOp) (parseTreeRec ops hs) (parseTreeRec ops ts)
    _             -> parseTreeRec (tail ops) xs
  where currentOp = head ops

toOperator :: String -> Operator
toOperator "+" = Plus
toOperator "*" = Times
toOperator "^" = Power
toOperator _   = error "Bad operator"

letters :: String -> [Char]
letters = L.nub . filter C.isUpper

solve :: String -> Maybe Combination
solve puzzle = do
  e <- parse puzzle
  testCombinations e (letters puzzle)
