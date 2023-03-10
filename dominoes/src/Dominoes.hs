module Dominoes (chain) where

import qualified Data.Maybe as MB

type Domino = (Int, Int)
data Zipper = Zipper [Domino] Domino [Domino]
data State = State Int [Domino]

-- | Compute a zipper traversal that shifts items on the left to the right.
travel :: Zipper -> [Zipper]
travel z@(Zipper [] _ _) = [z]
travel z@(Zipper (l : ls) x rs) = z : travel (Zipper ls l (x : rs))

-- | Entrypoint to the search procedure, which recall is searching for a chain of dominoes.
search :: Zipper -> Maybe [Domino]
search (Zipper [] d []) = Just [d]
search (Zipper (l : ls) (a, b) []) = 
search (Zipper [] d (r : rs)) = Nothing
search (Zipper (l : ls) d (r : rs)) = Nothing

searchWithStart :: Zipper -> Maybe [Domino]

-- | Recursive searching function, to which we give our starting point, and the value

chain :: [Domino] -> Maybe [Domino]
chain [] = Just []
chain (d : ds) = (MB.listToMaybe . MB.mapMaybe search . travel) (Zipper ds d [])
