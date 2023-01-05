module Sublist (sublist) where

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
sublist' xs (y:ys) = take (length xs) (y:ys) == xs || sublist' xs ys
