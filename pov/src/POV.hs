module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree)

-- | Given a tree, return a tree with the root at the given node.
--
-- We do this by finding the node `x` and then flipping the direction of every
-- edge from `x` to the root.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = 

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = error "You need to implement this function."
