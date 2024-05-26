module Satellite (treeFromTraversals) where

import Data.List (sort)
import Data.Containers.ListUtils (nubOrd)

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] _ = Nothing
treeFromTraversals _ [] = Nothing
treeFromTraversals [x] [y]
  | x == y = Just (Branch Leaf x Leaf)
  | otherwise = Nothing
treeFromTraversals (x:xs) ys
  | length (nubOrd ys) /= length ys = Nothing
  | sort (x:xs) /= sort ys = Nothing
  | otherwise = Branch <$> l <*> Just x <*> r
    where
      inL = takeWhile (/= x) ys
      inR = drop 1 $ dropWhile (/= x) ys
      preL = take (length inL) xs
      preR = drop (length inL) xs
      l = treeFromTraversals preL inL
      r = treeFromTraversals preR inR
