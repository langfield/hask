module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

data BinTree a = BT a (Maybe (BinTree a)) (Maybe (BinTree a))
  deriving (Eq, Show)

data Path a = L a (Maybe (BinTree a))
            | R a (Maybe (BinTree a))
            deriving (Eq, Show)

-- A zipper is a binary tree and a path from the tree to the root.
--
-- The tree is one of:
-- * root
-- * left subtree
-- * right subtree
--
-- If it is a root, the path is [].
-- If it is left subtree, head of path is (L x r)
--    r is its twin, the right subtree
--    x is the value at the parent
-- If it is right subtree, head of path is (R x l)
--    l is its twin, the left subtree
--    x is the value at the parent
data Zipper a = Zipper (BinTree a) [Path a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

toTree :: Zipper a -> BinTree a
toTree (Zipper t _) = t

value :: Zipper a -> a
value (Zipper (BT x _ _) _) = x

left :: Zipper a -> Maybe (Zipper a)
left = undefined

right :: Zipper a -> Maybe (Zipper a)
right = undefined

up :: Zipper a -> Maybe (Zipper a)
up = undefined

setValue :: a -> Zipper a -> Zipper a
setValue = undefined

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft = undefined

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight = undefined
