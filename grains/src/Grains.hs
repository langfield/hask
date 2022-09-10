module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n <= 0 = Nothing
  | n > 64 = Nothing
  | otherwise = Just (2 ^ (n - 1))

total :: Integer
total =
  case result of
    Just xs -> sum xs
    Nothing -> 0
  where result = sequence $ map square [1..64]
