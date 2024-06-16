module Affine (decode, encode) where

import Data.Char (chr, ord, isAlphaNum, isDigit, toLower)

transform :: (Int -> Int) -> String -> String
transform f = map go . filter isAlphaNum
  where
    go c
      | isDigit c = c
      | otherwise = chr . (+97) . f . (\i -> i-97) . ord . toLower $ c

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

decode :: (Int, Int) -> String -> Maybe String
decode (a,b) cipherText
  | gcd a 26 == 1 = Just . transform (\y -> (aInv * (y - b)) `mod` 26) $ cipherText
  | otherwise = Nothing
  where
    aInv = until (\x -> a*x `mod` 26 == 1) (+1) 1

encode :: (Int, Int) -> String -> Maybe String
encode (a,b) plainText
  | gcd a 26 == 1 = Just . unwords . chunksOf 5 . transform (\i -> (a * i + b) `mod` 26) $ plainText
  | otherwise = Nothing
