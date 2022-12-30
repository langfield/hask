module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.Maybe (listToMaybe)

smallestPalindrome :: Int -> Int -> Maybe (Int, [(Int, Int)])
smallestPalindrome begin end =
  listToMaybe
    $ filter (\(p, fs) -> isPalindromeNum p && fs /= [])
    $ map factor [begin * begin .. end * end]
  where
    factor :: Int -> (Int, [(Int, Int)])
    factor p = (p, factorize begin end p)

largestPalindrome :: Int -> Int -> Maybe (Int, [(Int, Int)])
largestPalindrome minFactor maxFactor = select palis
  where
    select [] = Nothing
    select (p : ps) = Just $ optimize p ps
    palis =
      map multiply $ filter isPalindrome $ factors [minFactor .. maxFactor]
    isPalindrome (a, b) = isPalindromeNum (a * b)
    multiply (a, b) = (a * b, [(a, b)])
    optimize old [] = old
    optimize old@(acc, fs) (new@(p, fs') : ps) = case compare acc p of
      GT -> optimize old ps
      LT -> optimize new ps
      EQ -> optimize (acc, fs ++ fs') ps

-- | Get all distinct pairs of values in `xs`, where pairs are always sorted.
factors :: Ord a => [a] -> [(a, a)]
factors xs = [ (a, b) | a <- xs, b <- xs, a <= b ]

-- | Get factors of `n` in the range `(begin, end)` (inclusive).
factorize :: Int -> Int -> Int -> [(Int, Int)]
factorize begin end n = if begin == 1 then (1, n) : fs else fs
  where
    fs =
      [ (a, b)
      | a <- [begin .. end]
      , b <- [begin .. end]
      , a * b == n && a <= b
      ]

isPalindromeNum :: Int -> Bool
isPalindromeNum n = show n == reverse (show n)
