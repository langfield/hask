module Bob (responseFor) where

import Debug.Trace (trace)
import Data.Char (isUpper, isAlpha, isSpace)

isQuestion :: String -> Bool
isQuestion [] = False
isQuestion s  = last s == '?'

isYell :: String -> Bool
isYell text = any isAlpha text && all isUpper (filter isAlpha text)

responseForQuestion :: String -> String
responseForQuestion question
  | isYell question = "Calm down, I know what I'm doing!"
  | otherwise = "Sure."


responseFor :: String -> String
responseFor text = responseForStripped $ filter isSpace (trace ("Input: " ++ text) text)


responseForStripped :: String -> String
responseForStripped text
  | dropWhile isSpace text == "" = "Fine. Be that way!"
  | isQuestion text = responseForQuestion text
  | isYell text = "Whoa, chill out!"
  | otherwise = "Whatever."
