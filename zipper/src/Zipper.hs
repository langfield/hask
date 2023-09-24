module Zipper (
    BinTree(..),
    Zipper,
    fromTree,
    toTree,
    value,
    left,
    right,
    up,
    setValue,
    setLeft,
    setRight
) where
import Data.List (foldl')

-- | A binary tree.
data BinTree a = BT { 
    btValue :: a                 -- ^ Value
  , btLeft  :: Maybe (BinTree a) -- ^ Left child
  , btRight :: Maybe (BinTree a) -- ^ Right child
} deriving (Eq, Show)

type T a = (a, Maybe (BinTree a))

-- | A zipper for a binary tree.
data Zipper a = Zipper 
      { zValue :: a
      , zLeft  :: Maybe (BinTree a)
      , zRight :: Maybe (BinTree a)
      , zTop   :: [Either (T a) (T a)]
      } deriving (Show, Eq)

-- | Get a zipper focussed on the root node.
fromTree :: BinTree a -> Zipper a
fromTree (BT x l r) = Zipper x l r []

-- | Get the complete tree from a zipper.
toTree :: Zipper a -> BinTree a
toTree (Zipper x l r t) = foldl' go (BT x l r) t
  where
    go b (Left (x',l')) = BT x' l' (Just b)
    go b (Right (x',r')) = BT x' (Just b) r'

-- | Get the value of the focus node.
value :: Zipper a -> a
value = zValue

-- | Get the left child of the focus node, if any.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper a l r t) = fmap (\(BT x  l' r') -> Zipper x l' r' (Right (a,r):t)) l

-- | Get the right child of the focus node, if any.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper a l r t) = fmap (\(BT x l' r') -> Zipper x l' r' (Left (a,l):t)) r

-- | Get the parent of the focus node, if any.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ _ _ []) = Nothing
up (Zipper a l r (Left (x,l'):ts))  = Just $ Zipper x l' (Just $ BT a l r) ts
up (Zipper a l r (Right (x,r'):ts)) = Just $ Zipper x (Just $ BT a l r) r' ts

-- | Set the value of the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue x z = z{zValue = x}

-- | Replace a left child tree.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l z = z{zLeft = l} 

-- | Replace a right child tree.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r z = z{zRight = r}
