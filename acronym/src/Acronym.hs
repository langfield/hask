module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = matchFirst ' '

matchFirst :: Char -> String -> String
matchFirst _ "" = ""
matchFirst p (c:cs)
  | p `elem` " _-" && isAlpha c = toUpper c : matchFirst c cs
  | isLower p && isUpper c = toUpper c : matchFirst c cs
  | otherwise = matchFirst c cs
