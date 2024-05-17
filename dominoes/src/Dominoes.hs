module Dominoes (chain) where

import Data.List (delete)
import Data.Tuple (swap)

import qualified Data.Maybe as MB

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain []            = Just []
chain ((l, r) : xs) = ((l, r) :) <$> chain' l r xs

-- | Compute a domino chain whose first number is `l`.
chain' :: Int -> Int -> [(Int, Int)] -> Maybe [(Int, Int)]
-- Check that we match on the wrap-around.
chain' l r [] = if l == r then Just [] else Nothing
chain' l r xs = MB.listToMaybe $ MB.mapMaybe go xs
  where
    go :: (Int, Int) -> Maybe [(Int, Int)]
    go x@(l', r')
      -- Consume `l'` (which means we must match on `r'` next).
      | r == l' = (x :) <$> chain' l r' (delete x xs)
      -- Consume `r'` (which means we must match on `l'` next).
      | r == r' = (swap x :) <$> chain' l l' (delete x xs)
      | otherwise = Nothing
