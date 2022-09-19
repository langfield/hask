module Acronym
  ( abbreviate
  ) where

import Data.Char (isUpper, toUpper)

-- I figured I'd do this with ``String`` first, and then write one with ``Data.Text``.
abbreviate :: String -> String
abbreviate = concatMap abbreviateToken . words . normalize
  where
    normalize = map delimsToSpaces . removeBadPunc

-- Remove superfluous punctuation from a string.
removeBadPunc :: String -> String
removeBadPunc = filter (not . isBadPunc)
  where isBadPunc c = c `elem` ['\'', '_']

-- Convert delimiters to spaces.
delimsToSpaces :: Char -> Char
delimsToSpaces c = if c `elem` ['-', ','] then ' ' else c

-- Comments
-- ========
-- If the word is all uppercase, we need only the first letter.
-- Otherwise, we need all uppercase letters.
--
-- Do the tests properly disambiguate what we should do for the case
-- `GNUImage`? Should this be `GNUI` or `GI`?
--
-- I think it should be `GI`.
--
-- What about `aGNUImage`? Should that be `AGI` or `GNUI` or `AGNUI`?
--
-- I think it should be `AGI`.

-- As an aside, do we put spaces around cons, no spaces around cons, or does it
-- change depending on the case?
--
-- Spaces it is!
--

-- Abbreviate a single token, converting result to uppercase.
abbreviateToken :: String -> [Char]
abbreviateToken [] = []
abbreviateToken (c : cs)
  | allUpper (c : cs) || allLower (c : cs) = [toUpper c]
  | otherwise = filter isUpper (c : cs)
  where allUpper = all isUpper
        allLower = not . any isUpper
