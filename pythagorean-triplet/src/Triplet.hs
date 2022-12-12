module Triplet (tripletsWithSum) where

isSquare :: Int -> Bool
isSquare n = sq * sq == n
  where sq = intSqrt n

intSqrt :: Int -> Int
intSqrt n = floor $ sqrt $ (fromIntegral n :: Double)

sumToN :: Int -> (Int, Int, Int) -> Bool
sumToN n (a, b, c) = a + b + c == n

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = filter (sumToN n) [(a,b,intSqrt $ a^2 + b^2) | a<-[1..n `div` 2], b<-[a..n `div` 2], isSquare $ (a^2) + (b^2)]
