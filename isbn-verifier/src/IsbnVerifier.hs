module IsbnVerifier (isbn) where

import qualified Data.Char as C

isbn :: String -> Bool
isbn (c1 : c2 : c3 : c4 : c5 : c6 : c7 : c8 : c9 : c10 : "") = checkChars c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
isbn (c1 : '-' : c2 : c3 : c4 : '-' : c5 : c6 : c7 : c8 : c9 : '-' : c10 : "") = checkChars c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
isbn _ = False

checkChars :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bool
checkChars c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
  | (not . all C.isDigit) cs = False
  | otherwise =
    let
      [x1, x2, x3, x4, x5, x6, x7, x8, x9] = map C.digitToInt cs
    in
      case c10 of
        'X' -> checksum x1 x2 x3 x4 x5 x6 x7 x8 x9 10
        c -> C.isDigit c && checksum x1 x2 x3 x4 x5 x6 x7 x8 x9 (C.digitToInt c10)
  where cs = [c1, c2, c3, c4, c5, c6, c7, c8, c9]

checksum :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
checksum x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = (x1 * 10 + x2 * 9 + x3 * 8 + x4 * 7 + x5 * 6 + x6 * 5 + x7 * 4 + x8 * 3 + x9 * 2 + x10) `mod` 11 == 0

