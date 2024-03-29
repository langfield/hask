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

data Sibling a = L a (Maybe (BinTree a))
               | R a (Maybe (BinTree a))
               deriving (Eq, Show)

data Zipper a = Zipper (BinTree a) [Sibling a]
  deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

-- | Walk up to the root.
toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree
toTree (Zipper l (L x r : ss)) = toTree $ Zipper (BT x (Just l) r) ss
toTree (Zipper r (R x l : ss)) = toTree $ Zipper (BT x l (Just r)) ss

value :: Zipper a -> a
value (Zipper (BT x _ _) _) = x

-- | Move the zipper to the left child of current node (if it exists).
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT x (Just l) r) ss) = Just $ Zipper l (L x r : ss)
left _ = Nothing

-- | Move the zipper to the right child of current node (if it exists).
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT x l (Just r)) ss) = Just $ Zipper r (R x l : ss)
right _ = Nothing

-- | Move the zipper to the parent of current node.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper l (L x r : ss)) = Just $ Zipper (BT x (Just l) r) ss
up (Zipper r (R x l : ss)) = Just $ Zipper (BT x l (Just r)) ss
up (Zipper _ []) = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) xs) = Zipper (BT x l r) xs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT x _ r) xs) = Zipper (BT x l r) xs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT x l _) xs) = Zipper (BT x l r) xs
