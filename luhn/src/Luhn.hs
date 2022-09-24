module Luhn (isValid) where

import Data.Char (isAlpha)

isValid :: String -> Bool
isValid s
  | length s <= 1 = False
  | all isDigit t = isLuhn t
  | otherwise = False
  where t = filter (\c -> c != ' ') s


isLuhn :: String -> Bool
isLuhn t = 

doubleEveryOtherMod10 :: [Integer] -> [Integer]
doubleEveryOtherMod10 [] = []
doubleEveryOtherMod10 [x] = [x]
doubleEveryOtherMod10 (x : s : xs) = x : 2 * s `mod` 10 : doubleEveryOtherMod10 xs
