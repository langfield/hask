module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n = Just (thousands ++ hundreds ++ tens ++ ones)
  where
    ones = digitToNumeral 'I' 'V' 'X' $ n `mod` 10
    tens = digitToNumeral 'X' 'L' 'C' $ n `div` 10 `mod` 10
    hundreds = digitToNumeral 'C' 'D' 'M' $ n `div` 100 `mod` 10
    thousands = digitToNumeral 'M' 'D' 'M' $ n `div` 1000 `mod` 10


digitToNumeral :: Char -> Char -> Char -> Integer -> String
digitToNumeral one five _ 4 = [one, five]
digitToNumeral one _ ten 9 = [one, ten]
digitToNumeral one five _ n
  | n <= 0 = ""
  | n <= 3 = replicate (fromIntegral n) one
  | n >= 5 = five : replicate (fromIntegral (n - 5)) one
  | otherwise = ""
