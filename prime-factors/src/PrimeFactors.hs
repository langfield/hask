module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = calc n $ 2 : [3, 5 ..]

calc :: Integer -> [Integer] -> [Integer]
calc 1 _  = []
calc _ [] = []
calc n (x : xs)
  | mod n x == 0 = x : calc (div n x) (x : xs)
  | otherwise    = calc n xs
