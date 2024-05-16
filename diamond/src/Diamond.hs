module Diamond (diamond) where

import Data.Char (chr, ord)

diamond :: Char -> Maybe [String]
diamond c = mirrorBottom <$> half c


mirrorBottom :: [String] -> [String]
mirrorBottom [] = []
mirrorBottom xs = reverse (tail xs) ++ xs


half :: Char -> Maybe [String]
half c = map mirrorRow <$> quarter c


mirrorRow :: String -> String
mirrorRow "" = ""
mirrorRow cs = cs ++ tail (reverse cs)


quarter :: Char -> Maybe [String]
quarter 'A' = Just ["A"]
quarter c
  | n >= 65 && n <= 90 = (:) (c : rPad) . map (' ' :) <$> quarter (chr (n - 1))
  | otherwise = Nothing
  where
    n = ord c
    k = n - 65
    rPad = replicate k ' '
