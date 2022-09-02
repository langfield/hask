module Bob (responseFor) where

import Data.Char (isUpper, isAlpha, isSpace)

isQuestion :: String -> Bool
isQuestion text = not (null text) && last (rstrip text) == '?'
  where rstrip = reverse . dropWhile isSpace . reverse

isYell :: String -> Bool
isYell text = any isAlpha text && all isUpper (filter isAlpha text)

responseFor :: String -> String
responseFor text
  | dropWhile isSpace text == "" = "Fine. Be that way!"
  | isQuestion text && isYell text = "Calm down, I know what I'm doing!"
  | isQuestion text = "Sure."
  | isYell text = "Whoa, chill out!"
  | otherwise = "Whatever."
