module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong x = digitsToPowerHasSum x n x
  where
    n = getNumDigits x
  

getNumDigits :: Integral a => a -> Int
getNumDigits x
  | abs x < 10 = 1
  | otherwise = getNumDigits (x `div` 10) + 1


digitsToPowerHasSum :: Integral a => a -> Int -> a -> Bool
digitsToPowerHasSum x n t
  | abs x < 10 = x ^ n == t
  | otherwise = digitsToPowerHasSum (x `div` 10) n $ t - (ones ^ n)
  where
    ones = x `mod` 10
