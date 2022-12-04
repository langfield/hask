module CustomSet
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

delete :: Ord a => a -> Tree a -> Tree a
delete _ E = E
delete x t = makeBlack $ del x t
  where
    makeBlack (T _ a y b) = T B a y b
    makeBlack E = E

-- Delete with consecutive red nodes at the top which is rectified in `delete`.
del :: (Ord a) => a -> Tree a -> Tree a
del _ E = E
del x t@(T _ a y b)
  | x < y = delL x t
  | x > y = delR x t
  | otherwise = fuse a b
 
-- Delete `x` from the left child of a tree.
delL :: Ord a => a -> Tree a -> Tree a
-- If the left child is black, delete from it and make the tree black, then
-- rebalance.
delL x (T _ a@(T B _ _ _) y b) = balL $ T B (del x a) y b
-- Otherwise, delete from it and make the tree red.
delL x (T _ a y b) = T R (del x a) y b

-- Rebalance a black tree after we've deleted from a black left subtree.
balL :: Tree a -> Tree a
-- If the subtree is now red, swap colors.
balL (T B (T R a x b) y c) = T R (T B a x b) y c
-- Otherwise, if the right subtree is black, make it red and balance.
balL (T B a x (T B b y c)) = balance' (T B a x (T R b y c))
-- Otherwise, if the right subtree is red, we go fucking mental.
balL (T B a x (T R (T B b y c) z d@(T B e w f))) = T R (T B a x b) y (balance' (T B c z (T R e w f)))

-- Delete `x` from the right child of a tree.
delR :: Ord a => a -> Tree a -> Tree a
delR x (T _ a y b@(T B _ _ _)) = balR $ T B a y (del x b)
delR x (T _ a y b) = T R a y (del x b)

-- Rebalance a black tree after we've deleted from a black right subtree.
balR :: Tree a -> Tree a
-- If right subtree is red, swap colors with root.
balR (T B a x (T R b y c)) = (T R a x (T B b y c))
-- Otherwise, if left subtree is black, change it to red and balance.
balR (T B (T B a x b) y c) = balance' (T B (T R a x b) y c)
-- Otherwise, if left subtree is red, go crazy.
balR (T B (T R (T B a x b) y (T B c z d)) w e) = T R (balance' (T B (T R a x b) y c)) z (T B d w e)

-- When we actually find `x` in `del`, it is the root of some tree, and we must
-- remove it and `fuse` its subtrees into a single replacement.
fuse :: Tree a -> Tree a -> Tree a
fuse E t = t
fuse t E = t
fuse a@(T B _ _ _) (T R b x c) = T R (fuse a b) x c
fuse (T R a x b) c@(T B _ _ _) = T R a x (fuse b c)
fuse (T R a x b) (T R c y d) =
  case fuse b c of
    (T R s' z' t') -> (T R (T R a x s') z' (T R t' y d))
    bc@(T B _ _ _) -> (T R a x (T R bc y d))
fuse (T B a x b) (T B c y d) =
  case fuse b c of
    (T R i' z' j') -> (T R (T B a x i') z' (T B j' y d))
    bc@(T B _ _ _) -> balL (T B a x (T B bc y d))

difference :: Ord a => Tree a -> Tree a -> Tree a
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

balance' :: Tree a -> Tree a
balance' (T color a x b) = balance color a x b
