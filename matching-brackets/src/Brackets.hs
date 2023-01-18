module Brackets (arePaired) where

import qualified Data.List as L

closings :: [Char]
closings = "]})"

brackets :: [Char]
brackets = "[{(" ++ closings

brak :: Char -> Char
brak ']' = '['
brak '}' = '{'
brak ')' = '('
brak _   = 'a'

arePaired :: String -> Bool
arePaired cs = L.foldl' go [] cs == ""
  where
    go :: [Char] -> Char -> [Char]
    go s c
      | c `elem` brackets = case s of
        [] -> [c]
        (b : bs) -> if elem c closings && brak c == b then bs else c : b : bs
      | otherwise = s
