module Satellite (treeFromTraversals) where

import Data.List (sort)
import Data.Containers.ListUtils (nubOrd)

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] _ = Nothing
treeFromTraversals [x] [y]
  | x == y = Just (Branch Leaf x Leaf)
  | otherwise = Nothing
treeFromTraversals (x:xs) ys
  | sort (x:xs) /= sort (nubOrd ys) = Nothing
  | otherwise = Branch <$> treeFromTraversals preL inL <*> Just x <*> treeFromTraversals preR inR
    where
      inL = takeWhile (/= x) ys
      inR = drop 1 $ dropWhile (/= x) ys
      preL = take (length inL) xs
      preR = drop (length inL) xs
