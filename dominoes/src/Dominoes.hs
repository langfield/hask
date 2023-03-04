module Dominoes (chain) where

import qualified Data.Maybe as MB

data Zipper = Zipper [(Int, Int)] (Int, Int) [(Int, Int)]

-- | Compute a zipper traversal that shifts items on the left to the right.
travel :: Zipper -> [Zipper]
travel z@(Zipper [] _ _) = [z]
travel z@(Zipper (l : ls) x rs) = z : travel (Zipper ls l (x : rs))

search :: Zipper -> Maybe [(Int, Int)]
search (Zipper [] d []) = Just [d]
search (Zipper (x : xs) d []) = 

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain (d : ds) = (MB.listToMaybe . MB.mapMaybe search . travel) (Zipper ds d [])
