module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = and inclusions
  where lower = map toLower text
        inclusions = map (`elem` lower) ['a'..'z']
