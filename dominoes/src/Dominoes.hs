module Dominoes (chain, isCyclic, search, Domino) where

import Data.Tuple (swap)

import qualified Data.List as L
import qualified Data.Maybe as MB

import Debug.Trace (trace, traceM)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

type Domino = (Int, Int)

chain :: [Domino] -> Maybe [Domino]
chain = L.find isCyclic . search 0

isCyclic :: [Domino] -> Bool
isCyclic [] = True
isCyclic [(a, b)] = a == b
isCyclic ((a, _) : ds) = a == (snd . last $ ds)

prepend :: Domino -> [Domino] -> Maybe [Domino]
prepend (a, b) [] = Just [(a, b)]
prepend (a, b) ((a', b') : ds)
  | b == a'   = Just $ (a, b) : (a', b') : ds
  | otherwise = Nothing

search' :: Int -> [Domino] -> Domino -> [[Domino]]
search' _ [] _ = []
search' depth ds (a, b) = forwardResults ++ backwardResults
  where
    forwardResults = MB.mapMaybe (prepend (a, b)) . search (depth + 1) . delete' (a, b) $ ds
    backwardResults = MB.mapMaybe (prepend (b, a)) . search (depth + 1) . delete' (b, a) $ ds

search :: Int -> [Domino] -> [[Domino]]
search _ [] = [[]]
search depth ds = concatMap (search' depth ds) ds

-- Delete a swapped domino if no non-swapped version is found.
delete' :: Domino -> [Domino] -> [Domino]
delete' _ [] = []
delete' (a, b) ds@(_ : tl)
  | length ds' < length ds = ds'
  | length ds'' < length ds = ds''
  | otherwise = tl
  where
    ds'  = L.delete (a, b) ds
    ds'' = L.delete (b, a) ds
