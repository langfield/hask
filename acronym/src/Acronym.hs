module Acronym
  ( abbreviate
  ) where

import Data.Char (isLower, isUpper, toUpper)
import Data.Text as T

-- I figured I'd do this with ``String`` first, and then write one with ``Data.Text``.
abbreviate :: Text -> Text
abbreviate = T.concat . Prelude.map abbreviateToken . T.words . normalize
  where
    normalize = T.map delimsToSpaces . removeBadPunc :: Text -> Text

-- Remove superfluous punctuation from a string.
removeBadPunc :: Text -> Text
removeBadPunc = T.filter (not . isBadPunc)
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
abbreviateToken :: Text -> Text
abbreviateToken s
  | T.null s = T.empty
  | T.all isUpper s || T.all isLower s = T.singleton $ Data.Char.toUpper c
  | otherwise = T.filter isUpper s
  where c = T.head s
