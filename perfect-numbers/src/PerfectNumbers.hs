module PerfectNumbers (classify, Classification(..)) where

import Data.Set (Set, fromList)

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> Maybe (Set Int)
factors x 
  | x <= 0 = Nothing
  | otherwise = Just $ fromList $ filter (\y -> mod x y == 0) [1 .. x - 1]

aliquot :: Int -> Maybe Int
aliquot x = case factorSet of
  Nothing -> Nothing
  Just ys -> Just (sum ys)
  where factorSet = factors x

classify :: Int -> Maybe Classification
classify x
  | x <= 0 = Nothing
  | otherwise = case (aliquot x) of
    Nothing -> Nothing
    Just a -> classifyAliquotSum x a

classifyAliquotSum :: Int -> Int -> Maybe Classification
classifyAliquotSum x a
  | x <= 0 = Nothing
  | a == x = Just Perfect
  | a < x = Just Deficient
  | otherwise = Just Abundant
