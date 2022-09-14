module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper, isLower)
import Debug.Trace (trace)

traceAs :: Show a => String -> a -> a
traceAs name x = trace (name ++ ": " ++ show x) x

-- I figured I'd do this with ``String`` first, and then write one with ``Data.Text``.
abbreviate :: String -> String
abbreviate s = map (toUpper . head) humps
  where t       = preprocess s
        tokens  = words $ traceAs "input" t :: [String]
        humps   = concat $ map dehump tokens :: [String]

-- Check if a character is superfluous punctuation.
isUseless :: Char -> Bool
isUseless '\'' = True
isUseless '_' = True
isUseless _ = False

-- Convert delimiters to spaces.
normalizeDelimiters :: Char -> Char
normalizeDelimiters '-' = ' '
normalizeDelimiters ',' = ' '
normalizeDelimiters c   = c

preprocess :: String -> String
preprocess s = map normalizeDelimiters $ filter (not . isUseless) s

dehump :: String -> [String]
dehump []       = []
dehump (c : cs)
  | and $ map isUpper (c : cs) = [[c]]
  | isUpper c = upperHump c cs
  | otherwise = lowerHump c cs


upperHump :: Char -> String -> [String]
upperHump c cs = leadingCaps ++ [(lastCap : lower)] ++ (dehump rest)
  where upper = c : (takeWhile isUpper cs) :: String
        lower = takeWhile isLower (dropWhile isUpper cs) :: String
        rest = dropWhile isLower (dropWhile isUpper cs) :: String
        caps = map (\x -> [x]) upper :: [String]
        lastCap = (head . head . reverse) caps :: Char
        leadingCaps = reverse $ (tail . reverse) caps :: [String]

lowerHump :: Char -> String -> [String]
lowerHump c cs = [(c : lower)] ++ dehump rest
                  where lower = takeWhile isLower cs
                        rest  = dropWhile isLower cs
