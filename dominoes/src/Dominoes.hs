module Dominoes (chain, initState, isCyclic, search, insert, remove, State, Domino) where

import Data.Map (Map)
import Data.Tuple (swap)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as MB

import Debug.Trace (trace)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

type Domino = (Int, Int)
type State = ([Domino], Map Int [Int])

chain :: [Domino] -> Maybe [Domino]
chain = search . initState

initState :: [Domino] -> State
initState = foldr insert ([], M.empty)

isCyclic :: [Domino] -> Bool
isCyclic []  = True
isCyclic [(a, b)] = a == b
isCyclic ((a, _) : ds) = a == (snd . last $ ds)

search' :: State -> Domino -> Maybe [Domino]
search' state (a, b) =
  case search (remove (a, b) state) of
    Nothing -> Nothing
    Just [] -> Just [(a, b)]
    Just ((a', b') : ds) ->
      if b == a'
         then Just $ (a, b) : (a', b') : ds
         else Nothing

search :: State -> Maybe [Domino]
search ([], _) = Just []
search (ds, m) = MB.listToMaybe . MB.mapMaybe (search' (ds, m)) $ ds ++ map swap ds

insert :: Domino -> State -> State
insert (a, b) (ds, m) = ((a, b) : ds, M.insertWith (++) b [a] . M.insertWith (++) a [b] $ m)

remove :: Domino -> State -> State
remove (a, b) (ds, m) = (delete' (a, b) ds, M.alter (fmap (L.delete b)) a . M.alter (fmap (L.delete a)) b $ m)

-- Delete a swapped domino if no non-swapped version is found.
delete' :: Domino -> [Domino] -> [Domino]
delete' (a, b) ds
  | length ds == length ds' = L.delete (b, a) ds
  | otherwise = ds'
  where
    ds' = L.delete (a, b) ds
