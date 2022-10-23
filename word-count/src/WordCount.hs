module WordCount (wordCount) where

import Data.Char (toLower)

wordCount :: String -> [(String, Int)]
wordCount xs = foldr updateCounts [] $ wordsWhen (`elem` " ,\n!&@$%^:.") xs

updateCounts :: String -> [(String, Int)] -> [(String, Int)]
updateCounts s [] = [(normalize s, 1)]
updateCounts s ((word, n) : cs)
  | normalize s == word = (word, n + 1) : cs
  | otherwise = (word, n) : updateCounts s cs

normalize :: String -> String
normalize s
  | head s == '\'' && last s == '\'' = init $ tail t
  | otherwise = t
  where t = map toLower s

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'
