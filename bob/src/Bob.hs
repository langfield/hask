module Bob (responseFor) where

import Data.Char (isUpper, isAlpha, isSpace)

isQuestion :: String -> Bool
isQuestion [] = False
isQuestion s  = last s == '?'

isYell :: String -> Bool
isYell text = any isAlpha text && all isUpper (filter isAlpha text)

strip :: String -> String
strip = f . f
  where f = reverse . dropWhile isSpace

responseFor :: String -> String
responseFor text = responseForStripped $ strip text

responseForStripped :: String -> String
responseForStripped text
  | text == "" = "Fine. Be that way!"
  | isQuestion text && isYell text = "Calm down, I know what I'm doing!"
  | isQuestion text = "Sure."
  | isYell text = "Whoa, chill out!"
  | otherwise = "Whatever."
