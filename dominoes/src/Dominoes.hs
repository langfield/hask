module Dominoes (chain) where

import Control.Monad (msum)
import Data.List (delete)
import Data.Tuple (swap)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain (x@(l, r) : xs) = (x :) <$> chain' l r xs

-- | Compute a domino chain to which we can prepend `(l, r)`.
chain' :: Int -> Int -> [(Int, Int)] -> Maybe [(Int, Int)]
chain' l r [] = if l == r then Just [] else Nothing
chain' l r xs = msum $ map try xs
  where
    -- Try using `x` as the next domino (in both orientations).
    try :: (Int, Int) -> Maybe [(Int, Int)]
    try x@(l', r')
      | l' == r   = (x :) <$> chain' l r' (delete x xs)
      | r' == r   = (swap x :) <$> chain' l l' (delete x xs)
      | otherwise = Nothing
