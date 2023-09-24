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

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Path a = L a (Maybe (BinTree a))
            | R a (Maybe (BinTree a))
            deriving (Eq, Show)

data Zipper a = Zipper (BinTree a) [Path a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree (Zipper lt (L val r : xs)) = toTree $ Zipper (BT val (Just lt) r) xs
toTree (Zipper rt (R val l : xs)) = toTree $ Zipper (BT val l (Just rt)) xs

value :: Zipper a -> a
value (Zipper t _) = btValue t

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT val (Just lt) r) xs) = Just $ Zipper lt (L val r : xs)
left (Zipper (BT _ Nothing _) _) = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT val l (Just rt)) xs) = Just $ Zipper rt (R val l : xs)
right (Zipper (BT _ _ Nothing) _) = Nothing

up :: Zipper a -> Maybe (Zipper a)
up (Zipper lt (L val r : xs)) = Just $ Zipper (BT val (Just lt) r) xs
up (Zipper rt (R val l : xs)) = Just $ Zipper (BT val l (Just rt)) xs
up (Zipper _ []) = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue val (Zipper t xs) = Zipper t{ btValue = val } xs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper t xs) = Zipper t{ btLeft = l } xs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper t xs) = Zipper t{ btRight = r } xs
