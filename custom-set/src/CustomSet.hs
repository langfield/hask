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

red :: Tree a -> Tree a
red (TREE B ta x tb) = TREE R ta x tb
red t = t

blk :: Tree a -> Tree a
blk (TREE R ta x tb) = TREE B ta x tb
blk t = t

empty :: Tree a
empty = E

insert :: Ord a => a -> Tree a -> Tree a
insert x t = makeBlack $ ins x t
  where
    makeBlack :: Tree a -> Tree a
    makeBlack (TREE _ ta y tb) = TREE B ta y tb
    makeBlack t' = t'

ins :: Ord a => a -> Tree a -> Tree a
ins x (TREE color ta y tb)
  | x < y = balance (TREE color (ins x ta) y tb)
  | x == y = TREE color ta y tb
  | x > y = balance (TREE color ta y (ins x tb))
ins x E = TREE R E x E
ins _ t = t

balance :: Tree a -> Tree a
balance (TREE B (TREE R (TREE R ta x tb) y tc) z td) = TREE R (TREE B ta x tb) y (TREE B tc z td)
balance (TREE B (TREE R ta x (TREE R tb y tc)) z td) = TREE R (TREE B ta x tb) y (TREE B tc z td)
balance (TREE B ta x (TREE R (TREE R tb y tc) z td)) = TREE R (TREE B ta x tb) y (TREE B tc z td)
balance (TREE B ta x (TREE R tb y (TREE R tc z td))) = TREE R (TREE B ta x tb) y (TREE B tc z td)
balance (TREE color ta x tb) = TREE color ta x tb
balance E = E

fromList :: Ord a => [a] -> Tree a
fromList [] = E
fromList (x : xs) = insert x (fromList xs)

toList :: Tree a -> [a]
toList E = []
toList (TREE _ ta x tb) = x : (toList ta ++ toList tb)

delete :: (Show a, Ord a) => a -> Tree a -> Tree a
delete _ E = E
delete aa t = makeBlack $ del aa t
  where
    makeBlack (TREE _ u bb v) = TREE B u bb v
    makeBlack E = E

-- Delete with consecutive red nodes at the top which is rectified in `delete`.
del :: (Show a, Ord a) => a -> Tree a -> Tree a
del _ E = E
del x t@(TREE _ ta y tb)
  | x < y = delL x t
  | x > y = delR x t
  | otherwise = fuse ta tb

-- Delete `x` from the left child of a tree.
delL :: (Show a, Ord a) => a -> Tree a -> Tree a
-- If the left child is black, delete from it and make the tree black, then
-- rebalance.
delL x (TREE _ ta@(TREE B _ _ _) y tb) = balL $ TREE B (del x ta) y tb
-- Otherwise, delete from it and make the tree red.
delL x (TREE _ ta y tb) = TREE R (del x ta) y tb
delL _ E = E

-- Rebalance a black tree after we've deleted from a black left subtree.
balL :: Tree a -> Tree a
-- If the subtree is now red, swap colors.
balL (TREE B ta@(TREE R _ _ _) x tb) = TREE R (blk ta) x tb
-- Otherwise, if the right subtree is black, make it red and balance.
balL (TREE B ta x tb@(TREE B _ _ _)) = balance (TREE B ta x (red tb))
-- Otherwise, if the right subtree is red, we go fucking mental.
balL (TREE B t x (TREE R (TREE B taa y tbb) z tb@(TREE B _ _ _))) = TREE R (TREE B t x taa) y (balance (TREE B tbb z (red tb)))
balL t = t

-- Delete `x` from the right child of a tree.
delR :: (Show a, Ord a) => a -> Tree a -> Tree a
delR x (TREE _ ta y tb@(TREE B _ _ _)) = balR $ TREE B ta y (del x tb)
delR x (TREE _ ta y tb) = TREE R ta y (del x tb)
delR _ E = E

-- Rebalance a black tree after we've deleted from a black right subtree.
balR :: Tree a -> Tree a
-- If right subtree is red, swap colors of root and right subtree.
balR (TREE B ta x tb@(TREE R _ _ _)) = (TREE R ta x (blk tb))
-- Otherwise, if left subtree is black, change it to red and balance.
balR (TREE B ta@(TREE B _ _ _) x tb) = balance (TREE B (red ta) x tb)
-- Otherwise, if left subtree is red, go crazy.
balR (TREE B (TREE R ta@(TREE B _ _ _) x (TREE B tcc y tdd)) z t) = TREE R (balance (TREE B (red ta) x tcc)) y (TREE B tdd z t)
balR t = t

-- When we actually find `x` in `del`, it is the root of some tree, and we must
-- remove it and `fuse` its subtrees into a single replacement.
fuse :: Tree a -> Tree a -> Tree a
fuse E t = t
fuse t E = t
fuse t@(TREE B _ _ _) (TREE R ta x tb) = TREE R (fuse t ta) x tb
fuse (TREE R ta x tb) t@(TREE B _ _ _) = TREE R ta x (fuse tb t)
fuse (TREE R ta x tb) (TREE R tc y td) =
  case fuse tb tc of
    (TREE R taa z tbb) -> (TREE R (TREE R ta x taa) z (TREE R tbb y td))
    t@(TREE B _ _ _) -> (TREE R ta x (TREE R t y td))
    E -> E
fuse (TREE B ta x tb) (TREE B tc y td) =
  case fuse tb tc of
    (TREE R taa z tbb) -> (TREE R (TREE B ta x taa) z (TREE B tbb y td))
    t@(TREE B _ _ _) -> balL (TREE B ta x (TREE B t y td))
    E -> (TREE R (TREE B ta x td) y E)
  -- where t@(TREE _ taa z tbb) = fuse tb tc

difference :: (Show a, Ord a) => Tree a -> Tree a -> Tree a
difference ta tb = foldr (\x t -> delete x t) ta $ toList tb

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (TREE _ ta y tb)
  | x < y = member x ta
  | x == y = True
  | otherwise = member x tb

size :: Tree a -> Int
size t = (length . toList) t

intersection :: Ord a => Tree a -> Tree a -> Tree a
intersection ta = fromList . filter (\x -> member x ta) . toList

isDisjointFrom :: Ord a => Tree a -> Tree a -> Bool
isDisjointFrom ta tb =
  case intersection ta tb of
    E -> True
    _ -> False

isSubsetOf :: Ord a => Tree a -> Tree a -> Bool
isSubsetOf ta tb
  | (size . intersection ta) tb == size ta = True
  | otherwise = False

null :: Tree a -> Bool
null E = True
null _ = False

union :: Ord a => Tree a -> Tree a -> Tree a
union ta tb = fromList $ toList ta ++ toList tb
