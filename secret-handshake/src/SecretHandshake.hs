module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = if sixteens == 1 then reverse ops else ops
  where
    sixteens = (n `div` 16) `mod` 2
    ops = bits n ["wink", "double blink", "close your eyes", "jump"]

bits :: Int -> [String] -> [String]
bits _ [] = []
bits k (x : xs)
  | k `mod` 2 == 1 = x : rest
  | otherwise = rest
  where
    rest = bits (k `div` 2) xs
