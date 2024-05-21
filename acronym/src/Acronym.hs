module Acronym
  ( abbreviate
  ) where

import Data.Char (toUpper)

toSpaces :: [Char] -> String -> String
toSpaces delims = map (\c -> if c `elem` delims then ' ' else c)

cap1 :: String -> String
cap1 "" = ""
cap1 (c:cs) = toUpper c : cs

abbreviate :: String -> String
abbreviate = filter (`notElem` "', ")
           . concatMap (take 1)
           . words
           . toSpaces ['a'..'z']
           . unwords
           . map cap1
           . words
           . toSpaces "-_"
