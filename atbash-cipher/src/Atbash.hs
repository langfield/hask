module Atbash (decode, encode) where

import qualified Data.Char as C

invert :: String -> String
invert "" = ""
invert (c:cs) = go c : invert cs
  where
    go :: Char -> Char
    go d
      | C.isAlpha d = C.chr (25 - (C.ord d - C.ord 'a') + C.ord 'a')
      | otherwise = d

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

decode :: String -> String
decode s = invert $ concat $ words s

encode :: String -> String
encode s = unwords $ chunksOf 5 $ invert $ map C.toLower $ filter (`notElem` [' ', ',', '.']) s
