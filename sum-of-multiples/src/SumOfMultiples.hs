module SumOfMultiples (sumOfMultiples) where

import Data.Set (Set, fromList, unions, singleton)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ unions $ map (multiples limit) factors

multiples :: Integer -> Integer -> Set Integer
multiples _ 0 = singleton 0
multiples limit factor = fromList [0, factor .. limit - 1]
