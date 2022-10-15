module Prime (nth) where

import Data.List ((\\))
import Debug.Trace (trace)


traceAs :: Show a => String -> a -> a
traceAs desc x = trace (desc ++ ": " ++ show x) x


nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | n == 1 = Just 2
  | n == 2 = Just 3
  | otherwise = Just $ fromIntegral $ head $ reverse $ take n primes
  where primes = sundaramSieve $ traceAs "UB"  $ upperBoundNthPrime n
        

-- Compute all primes <= k.
sundaramSieve :: Int -> [Int]
sundaramSieve k = map (\x -> 2 * x + 1) $ [1..k] \\ bads
  where bads = filter ((<=) k) [i + j + 2 * i * j | j <- [1..k `div` 3], i <- [1..j]]


-- Get an upper bound for the nth prime.
upperBoundNthPrime :: Int -> Int
upperBoundNthPrime n = ceiling $ ((fromIntegral n) * loglogn * logn :: Double)
  where loglogn = (log . log . fromIntegral) n
        logn = (log . fromIntegral) n
