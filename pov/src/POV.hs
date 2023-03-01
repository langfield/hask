module POV (fromPOV, tracePathBetween) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Tree (Tree(..))

-- We will traverse the tree, searching for the value `x`. At each step, we
-- will rotate the tree such that the node we are moving to becomes the new
-- root.
--
-- We will do this by keeping a reference to the parent, which we add to the
-- list of children of the current node, keeping track of the 'original'
-- children. If the current node's value is `x`, we return it, otherwise, we
-- recurse in each one of the original children.

-- | Given a tree, return a tree with the root at the given node.
--
-- We do this by finding the node `x` and then flipping the direction of every
-- edge from `x` to the root.
fromPOV :: (Show a, Eq a) => a -> Tree a -> Maybe (Tree a)
fromPOV x = search x Nothing

search :: (Eq a, Show a) => a -> Maybe (Tree a) -> Tree a -> Maybe (Tree a)
search x Nothing node@(Node y children)
  | x == y = Just node
  | otherwise = (listToMaybe . mapMaybe (search x (Just node))) children
search x (Just (Node z siblings)) node@(Node y children)
  | x == y = Just (Node y (parent' : children))
  | otherwise = (listToMaybe . mapMaybe search') children
  where
    parent' = Node z $ filter (/= node) siblings
    search' child = search x (Just node') child
      where
        node' = Node y (parent' : [c | c <- children, c /= child])

getPath :: Eq a => a -> [a] -> Tree a -> Maybe [a]
getPath x ys (Node y [])
  | x == y = Just (y : ys)
  | otherwise = Nothing
getPath x ys (Node y children)
  | x == y = Just (y : ys)
  | otherwise = listToMaybe $ mapMaybe (getPath x (y : ys)) children

tracePathBetween :: (Show a, Eq a) => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = fmap reverse $ getPath to [] =<< fromPOV from tree
