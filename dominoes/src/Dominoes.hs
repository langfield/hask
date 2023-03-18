module Dominoes (chain) where

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Maybe as MB

type Domino = (Int, Int)
type DominoMap = Map Int [Int]
type State = (Set Domino, Map Int [Int])

-- | Recursive searching function, to which we give our starting point, and the value
chain :: [Domino] -> Maybe [Domino]
chain = search . initState

initState :: [Domino] -> State
initState ds = (s, m)
  where
    m = foldr insert M.empty ds
    s = S.fromList ds

search :: State -> Maybe [Domino]
search (s, m) = Nothing

insert :: Domino -> DominoMap -> DominoMap
insert (a, b) = M.insertWith (++) b [a] . M.insertWith (++) a [b]

remove :: Domino -> DominoMap -> DominoMap
remove (a, b) = M.alter (fmap (L.delete b)) a . M.alter (fmap (L.delete a)) b

remove' :: DominoMap -> Domino -> DominoMap
remove' m (a, b) = M.alter (fmap (L.delete b)) a . M.alter (fmap (L.delete a)) b $ m
