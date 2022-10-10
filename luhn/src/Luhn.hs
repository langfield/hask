module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid s = length t > 1 && all isDigit t && isLuhn t
  where t = filter (/= ' ') s

isLuhn :: String -> Bool
isLuhn t = total `mod` 10 == 0
  where total = sum $ mapOnOdds (bound . (*2)) $ reverse $ map digitToInt t

-- Map `f` only on elements of `xs` with odd indices.
mapOnOdds :: (a -> a) -> [a] -> [a]
mapOnOdds f = zipWith ($) (cycle [id, f])

bound :: Int -> Int
bound n
  | n > 9 = n - 9
  | otherwise = n
