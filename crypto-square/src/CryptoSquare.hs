module CryptoSquare (encode) where

import Data.Char (toLower)

encode :: String -> String
encode xs = error "You need to implement this function."

normalize :: String -> String
normalize = (map toLower) . removePunctuationAndSpaces

removePunctuationAndSpaces :: String -> String
removePunctuationAndSpaces = filter (not . isPunctuationOrSpace)
  where isPunctuationOrSpace c = c `elem` ['.', ',', '-', '_', ' ']

computeNumRows :: Int -> Int
computeNumRows n
  | n * (n + 1) >= n
