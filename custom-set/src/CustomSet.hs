module Tree
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving (Show)

delete :: a -> Tree a -> Tree a
delete _ E = E
delete x (T color a y b) = E

difference :: Tree a -> Tree a -> Tree a
difference setA setB = error "You need to implement this function."

empty :: Tree a
empty = E

fromList :: Ord a => [a] -> Tree a
fromList [] = E
fromList (x : xs) = insert x (fromList xs)

insert :: Ord a => a -> Tree a -> Tree a
insert x set = makeBlack $ ins x set
  where 
    ins :: Ord a => a -> Tree a -> Tree a
    ins x E = T R (E :: Tree a) x (E :: Tree a)
    ins x (T color a y b)
      | x < y = balance color (ins x a) y b
      | x == y = T color a y b
      | x > y = balance color a y (ins x b)
    makeBlack :: Tree a -> Tree a
    makeBlack (T _ a y b) = T B a y b
    makeBlack set = set

intersection :: Tree a -> Tree a -> Tree a
intersection setA setB = error "You need to implement this function."

isDisjointFrom :: Tree a -> Tree a -> Bool
isDisjointFrom setA setB = error "You need to implement this function."

isSubsetOf :: Tree a -> Tree a -> Bool
isSubsetOf setA setB = error "You need to implement this function."

member :: Ord a => a -> Tree a -> Bool
member x E = False
member x (T _ a y b)
  | x < y = member x a
  | x == y = True
  | otherwise = member x b

null :: Tree a -> Bool
null E = True
null _ = False

size :: Tree a -> Int
size set = (length . toList) set

toList :: Tree a -> [a]
toList E = []
toList (T color a x b) = x : (toList a ++ toList b)

union :: Tree a -> Tree a -> Tree a
union setA setB = error "You need to implement this function."

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b
