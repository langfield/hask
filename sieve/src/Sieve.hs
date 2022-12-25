module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding ((/), div, divMod, mod, quot, quotRem, rem)

import Data.Map (Map)
import qualified Data.Map as M

data Mark = Unknown | Composite deriving (Eq, Show)

sieve :: Integer -> Integer -> Map Integer Mark -> Map Integer Mark
sieve n k res
  | k > n = res
  | otherwise = sieve n (k + 1)
  $ foldr (M.adjust (const Composite)) res [2 * k, 3 * k .. n]

primesUpTo :: Integer -> [Integer]
primesUpTo n =
  ( M.keys
    . M.filter (== Unknown)
    . sieve n 2
    . M.fromList
    . zip [2 .. n]
    . repeat
    )
    Unknown
