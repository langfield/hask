module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.List (foldl')

data BST a = Empty
           | Node a (BST a) (BST a)
  deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node _ l _) = Just l
bstLeft _ = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Node _ _ r) = Just r
bstRight _ = Nothing

bstValue :: BST a -> Maybe a
bstValue (Node x _ _) = Just x
bstValue _ = Nothing

empty :: BST a
empty = Empty

-- Just keep inserting from left-to-right.
fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert y Empty = singleton y
insert y (Node x l r)
  | y <= x = Node x (insert y l) r
  | otherwise = Node x l (insert y r)

singleton :: Ord a => a -> BST a
singleton x = Node x Empty Empty

-- Put the root in the middle of the list, I guess...
toList :: BST a -> [a]
toList Empty = []
toList (Node x l r) = toList l ++ [x] ++ toList r
