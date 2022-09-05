module SumOfMultiples (sumOfMultiples) where

import Debug.Trace (trace)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ trace ("Combined: " ++ (show $ combineMultiples factors limit)) (combineMultiples factors limit)

combineMultiples :: [Integer] -> Integer -> [Integer]
combineMultiples factors limit = concat $ map (multiples limit) factors

multiples :: Integer -> Integer -> [Integer]
multiples limit factor = trace ("Multiples of " ++ (show factor) ++ " up to " ++ (show limit) ++ ": " ++ (show [factor * i | i <- [1..ub]])) [factor * i | i <- [1..(ub - 1)]]
  where ub = div limit factor
