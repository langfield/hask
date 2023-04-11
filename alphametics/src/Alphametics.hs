module Alphametics (solve) where

import Data.Map (Map)
import qualified Data.List as L
import qualified Data.Map as M


import Debug.Trace (trace)

data Node = Node (Char, Int) [Node]

ttt :: Show a => String -> a -> a
ttt s x = trace (s ++ ": " ++ show x) x

splitAtEq :: String -> (String, String)
splitAtEq s = (expression, result)
  where
    s' = reverse s
    result = (reverse . takeWhile (/= ' ')) s'
    expression = (reverse . dropWhile (`elem` " =") . dropWhile (/= ' ')) s'

splitIntoSummands :: String -> [String]
splitIntoSummands "" = []
splitIntoSummands s = summand : splitIntoSummands rest
  where
    summand = takeWhile (/= ' ') s
    rest = (dropWhile (`elem` " +") . dropWhile (/= ' ')) s

solve :: String -> Maybe [(Char, Int)]
solve puzzle = dfs $ buildGraph (M.empty, Node ' ' 0, Node ' ' 0) equations
  where
    (expression, result) = splitAtEq puzzle
    summands = splitIntoSummands expression
    columns = (L.transpose . map reverse) summands
    result' = reverse result
    equations = zip columns result'

dfs :: Node -> Maybe [(Char, Int)]
dfs (Node (c, i) []) = Just [(c, i)]
dfs (Node (c, i) (x : xs)) = Nothing

buildGraph :: Map Char Int -> [([Char], Char)] -> 
buildGraph ((cs, d) : es) = Node ('a', 0) []

-- Instead, can we represent the problem as a graph? Each node is a (letter,
-- digit) pair, an assignment of a digit value to some letter. We start at a
-- null node, and then walk from letter to letter in the order of the
-- grade-school addition algorithm. We skip letters that we've already
-- assigned, and we skip letters whose values are determined by the addition
-- algorithm (below the 'sum' line). When we reach a leaf, this either
-- represents an unsatisfiable set of constraints (where we include our digit
-- assigments as 'constraints'), or a finished problem. When we reach a letter
-- for which there are multiple possible assignments, we do DFS, treating each
-- possible assignment as a child of the previous node.
