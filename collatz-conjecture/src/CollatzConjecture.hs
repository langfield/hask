module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x
  | x == 1 = Just 0
  | even x = Just 1 + collatz $ div x 2
  | otherwise = Just 1 + collatz $ 3 * x + 1
