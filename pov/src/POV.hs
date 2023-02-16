module POV (fromPOV, tracePathBetween) where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.Tree (Tree (..))

-- | Given a tree, return a tree with the root at the given node.
--
-- We do this by finding the node `x` and then flipping the direction of every
-- edge from `x` to the root.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = Nothing
  where
    root' = search 

search :: Eq a => a -> Tree a -> Maybe (Tree a)
search x root@(Node y children)
  | x == y = Just root
  | otherwise = listToMaybe $ mapMaybe (search x) children

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = error "You need to implement this function."
