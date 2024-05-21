module Acronym (abbreviate) where

import Data.Char (isUpper, isLower, toUpper)

toSpaces :: [Char] -> String -> String
toSpaces delims = map (\c -> if c `elem` delims then ' ' else c)

unCamel :: String -> String
unCamel [] = []
unCamel [a] = [a]
unCamel (a:b:rest)
  | isUpper b && isLower a = a:' ':b:unCamel rest
  | otherwise = a:unCamel (b:rest)

abbreviate :: String -> String
abbreviate = map toUpper . concatMap (take 1) . words . toSpaces "-_" . unCamel 
