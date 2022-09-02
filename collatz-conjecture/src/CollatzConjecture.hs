module CollatzConjecture (collatz) where

import Control.Monad

collatz :: Integer -> Maybe Integer
collatz x
  | x <= 0 = Nothing
  | x == 1 = Just 0
  | even x = (+1) <$> (collatz $ div x 2)
  | otherwise = (+1) <$> (collatz $ 3 * x + 1)
