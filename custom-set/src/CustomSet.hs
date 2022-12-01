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
delete x (T color a y b) = error "You need to implement this function."

difference :: Tree a -> Tree a -> Tree a
difference setA setB = foldr (\b set -> delete b set) setA $ toList setB

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

intersection :: Ord a => Tree a -> Tree a -> Tree a
intersection setA = fromList . filter (\b -> member b setA) . toList

isDisjointFrom :: Ord a => Tree a -> Tree a -> Bool
isDisjointFrom setA setB =
  case intersection setA setB of
    E -> True
    _ -> False

isSubsetOf :: Ord a => Tree a -> Tree a -> Bool
isSubsetOf setA setB
  | (size . intersection setA) setB == size setA = True
  | otherwise = False

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

union :: Ord a => Tree a -> Tree a -> Tree a
union setA setB = fromList $ toList setA ++ toList setB

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b
