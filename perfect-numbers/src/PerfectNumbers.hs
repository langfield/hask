module PerfectNumbers (classify, Classification(..)) where

import Control.Monad
import Data.Functor

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

-- If n <= 0, return `Nothing`, else return the result of the case split. The
-- case split takes the aliquot sum, explained below, and compares it to n,
-- returning a classification.
--
-- The function `aliquotSum` first finds divisors by observing that a positive
-- integer k < n is a divisor of n if and only if `n mod k == 0`. Then it sums
-- those divisors.
classify :: Int -> Maybe Classification
classify n = 
  guard (n > 0) $> case compare aliquotSum n of
    LT -> Deficient
    EQ -> Perfect
    GT -> Abundant
  where aliquotSum = sum $ filter ((0 ==) . (n `mod`)) [1 .. n - 1]
