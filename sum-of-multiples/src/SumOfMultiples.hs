module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ foldr Set.union Set.empty $ map (multiples limit) factors

multiples :: Integer -> Integer -> Set.Set Integer
multiples _ 0 = Set.fromList [0]
multiples limit factor = Set.fromList [0, factor .. limit - 1]
