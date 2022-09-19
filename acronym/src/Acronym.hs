module Acronym
  ( abbreviate
  ) where

import Data.Char (isLower, isUpper, toUpper)

-- I figured I'd do this with ``String`` first, and then write one with ``Data.Text``.
abbreviate :: String -> String
abbreviate = upperHeads . concat . allHumps . words . normalize
  where
    upperHeads = map $ toUpper . head
    allHumps = map dehump
    removeBadPunc = filter (not . isBadPunc)
    normalize = map delimsToSpaces . removeBadPunc

-- Check if a character is superfluous punctuation.
isBadPunc :: Char -> Bool
isBadPunc '\'' = True
isBadPunc '_' = True
isBadPunc _ = False

-- Convert delimiters to spaces.
delimsToSpaces :: Char -> Char
delimsToSpaces '-' = ' '
delimsToSpaces ',' = ' '
delimsToSpaces c = c

-- Split a token into a list of PascalCase humps.
dehump :: String -> [String]
dehump [] = []
dehump (c:cs)
  | allUpper (c : cs) = [[c]]
  | isLower c = (c : lower) : dehump rest
  | otherwise = upperHump (c : cs)
  where
    allUpper = all isUpper
    lower = takeWhile isLower cs
    rest = dropWhile isLower cs

-- Handle case where first letter is uppercase.
upperHump :: String -> [String]
upperHump [] = [""]
upperHump (c:"") = [[c]]
upperHump (c:d:ds)
  | isUpper d = [c] : dehump (d : ds)
  | otherwise = (c : lower) : dehump rest
  where
    lower = takeWhile isLower (d : ds)
    rest = dropWhile isLower (d : ds)
