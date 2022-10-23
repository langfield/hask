module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = validate $ dropCountry $ filter isDigit xs

-- Drop the country code if it exists.
dropCountry :: String -> String
dropCountry ('1' : xs) = xs
dropCountry xs = xs

-- Validate a number without a country code.
validate :: String -> Maybe String
validate xs
  | length xs /= 10 = Nothing
  | head xs == '0' = Nothing
  | head xs == '1' = Nothing
  | otherwise =
    case xs of
      (_ : _ : _ : '0' : _) -> Nothing
      (_ : _ : _ : '1' : _) -> Nothing
      num -> Just num
