{-# LANGUAGE TupleSections #-}
module Anagram (anagramsFor) where

import Data.Map (Map, fromListWith)
import Data.Char (toUpper)

-- Return all case-insensitive anagrams for ``xs``, excluding case-insensitive
-- exact matches.
anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagram

-- Check if ``s`` is a case-insensitive, distinct anagram of ``t``.
isAnagram :: String -> String -> Bool
isAnagram s t = (sCounts == tCounts) && not (equalCaseInsensitive s t)
  where sCounts = count s
        tCounts = count t

-- Check case-insensitive equality of strings.
equalCaseInsensitive :: String -> String -> Bool
equalCaseInsensitive s t = map toUpper s == map toUpper t

-- Map case-insensitive characters to counts given a string, preferring
-- uppercase letters as keys.
count :: String -> Map Char Int
count s = fromListWith (+) (map ((,1) . toUpper) s)
