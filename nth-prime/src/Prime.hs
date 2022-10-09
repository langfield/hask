module Prime (nth) where

import GHC.Float (int2Float)


nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise = primes !! (n - 1)
  where ub = upperBoundNthPrime n
        primes = sievePrime ub [1..ub] 2
        


sievePrime :: Int -> [Int] -> Int -> [Int]
sievePrime ub xs p
  | p >= ub = xs
  | otherwise = sievePrime ub ys q
  where ys = filter (\x -> x `mod` p /= 0) xs
        q = head $ dropWhile (<= p) ys


upperBoundNthPrime :: Int -> Maybe Integer
upperBoundNthPrime n
  | n <= 0 = Nothing
  | otherwise = Just $ ceiling $ (fromIntegral n) * (log (int2Float n) * log (log (int2Float n)))
