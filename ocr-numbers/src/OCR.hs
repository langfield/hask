module OCR (convert) where

import qualified Data.List as L

convert :: String -> String
convert = L.intercalate "," . map (convertRow . L.intercalate "\n") . chunksOf 4 . lines

convertRow :: String -> String
convertRow = map convertDigit . L.transpose . map (chunksOf 3) . init . lines

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

convertDigit :: [String] -> Char
convertDigit xs
  | length xs /= 3 = '?'
  | otherwise =
    case xs of
      [x, y, z] -> convertPieces x y z
      _ -> '?'

convertPieces :: String -> String -> String -> Char
convertPieces " _ " "| |" "|_|" = '0'
convertPieces "   " "  |" "  |" = '1'
convertPieces " _ " " _|" "|_ " = '2'
convertPieces " _ " " _|" " _|" = '3'
convertPieces "   " "|_|" "  |" = '4'
convertPieces " _ " "|_ " " _|" = '5'
convertPieces " _ " "|_ " "|_|" = '6'
convertPieces " _ " "  |" "  |" = '7'
convertPieces " _ " "|_|" "|_|" = '8'
convertPieces " _ " "|_|" " _|" = '9'
convertPieces _ _ _ = '?'
