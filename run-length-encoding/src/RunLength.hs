module RunLength (decode, encode) where

import qualified Data.Char as C
import qualified Data.List as L

decode :: String -> String
decode "" = ""
decode s@(c : cs)
  | C.isDigit c =
    let
      count = read $ L.takeWhile (C.isDigit) s :: Int
      rest = L.dropWhile C.isDigit s
    in
      case rest of
        "" -> ""
        (x : xs) -> (replicate count x) ++ decode xs
  | otherwise = c : decode cs

encode :: String -> String
encode "" = ""
encode cs = encodeWithCount cs 1


encodeWithCount :: String -> Int -> String
encodeWithCount "" _ = ""
encodeWithCount (c : []) 1 = [c]
encodeWithCount (c : []) n = show n ++ [c]
encodeWithCount (a : b : bs) n
  | a == b = encodeWithCount (b : bs) (n + 1)
  | otherwise =
    case n of
      1 -> [a] ++ encodeWithCount (b : bs) 1
      k -> show k ++ [a] ++ encodeWithCount (b : bs) 1
