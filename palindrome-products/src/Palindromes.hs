module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.Maybe (listToMaybe)

import qualified Data.List as L

isPalindrome :: (Integer, Integer) -> Bool
isPalindrome (a, b) = (test . show) (a * b)
  where
    test :: String -> Bool
    test s = s == reverse s

unpack :: (Integer, Integer) -> (Integer, (Integer, Integer))
unpack (a, b) = (a * b, (a, b))

sameProduct :: (Integer, (Integer, Integer))
            -> (Integer, (Integer, Integer))
            -> Bool
sameProduct (a, _) (b, _) = a == b

accumFactors :: [(Integer, (Integer, Integer))]
             -> (Integer, [(Integer, Integer)])
accumFactors [] = (-1, [])
accumFactors ((prod, (a, b)) : xs) = (prod, (a, b) : map snd xs)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  ( listToMaybe
    . map accumFactors
    . L.groupBy sameProduct
    . reverse
    . L.sort
    . map unpack
    . filter isPalindrome
    )
    [ (a, b) | a <- [minFactor .. maxFactor], b <- [minFactor .. maxFactor] ]

smallestPalindrome :: Integer
                   -> Integer
                   -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  ( listToMaybe
    . map accumFactors
    . L.groupBy sameProduct
    . L.sort
    . map unpack
    . filter isPalindrome
    )
    [ (a, b) | a <- [minFactor .. maxFactor], b <- [minFactor .. maxFactor] ]
