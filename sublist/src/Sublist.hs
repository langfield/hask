module Sublist (sublist) where

import qualified Data.List as L

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ
  | sublist' xs ys = Just LT
  | sublist' ys xs = Just GT
  | otherwise = Nothing

-- Determine if the first list is a sublist of the second list.
sublist' :: Eq a => [a] -> [a] -> Bool
sublist' [] _ = True
sublist' _ [] = False
sublist' xs (y:ys) = L.isPrefixOf xs (y:ys) || sublist' xs ys
