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
  , Color (..)
  , Tree (..)
  ) where

import Prelude hiding (null)
import Debug.Trace (trace)

traceAs :: Show a => String -> a -> a
traceAs s x = trace (s ++ ": " ++ show x) x

data Color = R | B deriving (Show, Eq)
data Tree a = E | TREE Color (Tree a) a (Tree a) deriving Show

instance (Show a, Ord a) => Eq (Tree a) where
  (==) a b =
    case (difference a b, difference b a) of
      (E, E) -> True
      (_, _) -> False

delete :: (Show a, Ord a) => a -> Tree a -> Tree a
delete _ E = E
delete aa t = makeBlack $ del aa t
  where
    makeBlack (TREE _ u bb v) = TREE B u bb v
    makeBlack E = E

-- Delete with consecutive red nodes at the top which is rectified in `delete`.
del :: (Show a, Ord a) => a -> Tree a -> Tree a
del _ E = E
del x t@(TREE _ a y b)
  | x < y = delL x t
  | x > y = delR x t
  | otherwise = fuse a b

-- Delete `x` from the left child of a tree.
delL :: (Show a, Ord a) => a -> Tree a -> Tree a
-- If the left child is black, delete from it and make the tree black, then
-- rebalance.
delL x (TREE _ a@(TREE B _ _ _) y b) = balL $ TREE B (del x a) y b
-- Otherwise, delete from it and make the tree red.
delL x (TREE _ a y b) = TREE R (del x a) y b
delL _ E = E

-- Rebalance a black tree after we've deleted from a black left subtree.
balL :: Tree a -> Tree a
-- If the subtree is now red, swap colors.
balL (TREE B (TREE R a x b) y c) = TREE R (TREE B a x b) y c
-- Otherwise, if the right subtree is black, make it red and balance.
balL (TREE B a x (TREE B b y c)) = balance (TREE B a x (TREE R b y c))
-- Otherwise, if the right subtree is red, we go fucking mental.
balL (TREE B a x (TREE R (TREE B b y c) z (TREE B e w f))) = TREE R (TREE B a x b) y (balance (TREE B c z (TREE R e w f)))
balL t = t

-- Delete `x` from the right child of a tree.
delR :: (Show a, Ord a) => a -> Tree a -> Tree a
delR x (TREE _ a y b@(TREE B _ _ _)) = balR $ TREE B a y (del x b)
delR x (TREE _ a y b) = TREE R a y (del x b)
delR _ E = E

-- Rebalance a black tree after we've deleted from a black right subtree.
balR :: Tree a -> Tree a
-- If right subtree is red, swap colors with root.
balR (TREE B a x (TREE R b y c)) = (TREE R a x (TREE B b y c))
-- Otherwise, if left subtree is black, change it to red and balance.
balR (TREE B (TREE B a x b) y c) = balance (TREE B (TREE R a x b) y c)
-- Otherwise, if left subtree is red, go crazy.
balR (TREE B (TREE R (TREE B a x b) y (TREE B c z d)) w e) = TREE R (balance (TREE B (TREE R a x b) y c)) z (TREE B d w e)
balR t = t

-- When we actually find `x` in `del`, it is the root of some tree, and we must
-- remove it and `fuse` its subtrees into a single replacement.
fuse :: Tree a -> Tree a -> Tree a
fuse E t = t
fuse t E = t
fuse a@(TREE B _ _ _) (TREE R b x c) = TREE R (fuse a b) x c
fuse (TREE R a x b) c@(TREE B _ _ _) = TREE R a x (fuse b c)
fuse (TREE R a x b) (TREE R c y d) =
  case fuse b c of
    (TREE R s' z' t') -> (TREE R (TREE R a x s') z' (TREE R t' y d))
    bc@(TREE B _ _ _) -> (TREE R a x (TREE R bc y d))
    E -> E
fuse (TREE B a x b) (TREE B c y d) =
  case fuse b c of
    (TREE R i' z' j') -> (TREE R (TREE B a x i') z' (TREE B j' y d))
    bc@(TREE B _ _ _) -> balL (TREE B a x (TREE B bc y d))
    E -> (TREE R (TREE B a x d) y E)

difference :: (Show a, Ord a) => Tree a -> Tree a -> Tree a
difference setA setB = foldr (\b set -> delete b set) setA $ toList setB

empty :: Tree a
empty = E

fromList :: Ord a => [a] -> Tree a
fromList [] = E
fromList (x : xs) = insert x (fromList xs)

ins :: Ord a => a -> Tree a -> Tree a
ins x (TREE color a y b)
  | x < y = balance (TREE color (ins x a) y b)
  | x == y = TREE color a y b
  | x > y = balance (TREE color a y (ins x b))
ins x E = TREE R E x E
ins _ t = t

insert :: Ord a => a -> Tree a -> Tree a
insert z set = makeBlack $ ins z set
  where 
    makeBlack :: Tree a -> Tree a
    makeBlack (TREE _ a y b) = TREE B a y b
    makeBlack t = t

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
member _ E = False
member x (TREE _ a y b)
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
toList (TREE _ a x b) = x : (toList a ++ toList b)

union :: Ord a => Tree a -> Tree a -> Tree a
union setA setB = fromList $ toList setA ++ toList setB


balance :: Tree a -> Tree a
balance (TREE B (TREE R (TREE R u aa v) bb w) cc x) = TREE R (TREE B u aa v) bb (TREE B w cc x)
balance (TREE B (TREE R u aa (TREE R v bb w)) cc x) = TREE R (TREE B u aa v) bb (TREE B w cc x)
balance (TREE B u aa (TREE R (TREE R v bb w) cc x)) = TREE R (TREE B u aa v) bb (TREE B w cc x)
balance (TREE B u aa (TREE R v bb (TREE R w cc x))) = TREE R (TREE B u aa v) bb (TREE B w cc x)
balance (TREE color u aa v) = TREE color u aa v
balance E = E
