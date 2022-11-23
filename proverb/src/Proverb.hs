module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite (x : xs) = intercalate "\n" (zipWith forWant (x : xs) xs ++ ["And all for the want of a " ++ x ++ "."])

forWant :: String -> String -> String
forWant a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
