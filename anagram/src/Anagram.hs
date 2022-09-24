module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort, group)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = map fst $ filter (isAnagram target) maps
  where actualCounts = map mapCounts xss
        targetCounts = mapCounts xs

isAnagram :: (String, [(Char, Int)]) -> (String, [(Char, Int)]) -> Bool
isAnagram (t, tmap) (s, smap)
  | map toLower t == (map toLower s) = False
  | otherwise = tmap == smap

mapCounts :: String -> (String, [(Char, Int)])
mapCounts "" = ("", [])
mapCounts s = (s, (map (\(x : xs) -> (x, length xs + 1)) . group . sort) low)
  where low = map toLower s
