module Dominoes (chain, initState, isCyclic, search, insert, remove, State, Domino) where

import Data.Map (Map)
import Data.Tuple (swap)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as MB

import Debug.Trace (trace, traceM)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

type Domino = (Int, Int)
type State = ([Domino], Map Int [Int])

chain :: [Domino] -> Maybe [Domino]
chain = L.find isCyclic . search 0 . initState

initState :: [Domino] -> State
initState = foldr insert ([], M.empty)

isCyclic :: [Domino] -> Bool
isCyclic [] = True
isCyclic [(a, b)] = a == b
isCyclic ((a, _) : ds) = a == (snd . last $ ds)

prepend :: Domino -> [Domino] -> Maybe [Domino]
prepend (a, b) [] = Just [(a, b)]
prepend (a, b) ((a', b') : ds)
  | b == a'   = Just $ (a, b) : (a', b') : ds
  | otherwise = Nothing

search' :: Int -> State -> Domino -> [[Domino]]
search' depth state (a, b) = MB.mapMaybe (prepend (a, b)) . search (depth + 1) . remove (a, b) $ state

search :: Int -> State -> [[Domino]]
search _     ([], _) = [[]]
search depth (ds, m) = do
  -- traceM $ "Dominoes: " ++ show ds
  concatMap (search' depth (ds, m)) $ ds ++ map swap ds

insert :: Domino -> State -> State
insert (a, b) (ds, m) = ((a, b) : ds, M.insertWith (++) b [a] . M.insertWith (++) a [b] $ m)

remove :: Domino -> State -> State
remove (a, b) (ds, m) = (delete' (a, b) ds, M.alter (fmap (L.delete b)) a . M.alter (fmap (L.delete a)) b $ m)

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
