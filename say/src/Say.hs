module Say (inEnglish) where

import Control.Monad (liftM2)

inEnglish :: Integer -> Maybe String
inEnglish 0 = Just "zero"
inEnglish 1 = Just "one"
inEnglish 2 = Just "two"
inEnglish 3 = Just "three"
inEnglish 4 = Just "four"
inEnglish 5 = Just "five"
inEnglish 6 = Just "six"
inEnglish 7 = Just "seven"
inEnglish 8 = Just "eight"
inEnglish 9 = Just "nine"
inEnglish 10 = Just "ten"
inEnglish 11 = Just "eleven"
inEnglish 12 = Just "twelve"
inEnglish 13 = Just "thirteen"
inEnglish 14 = Just "fourteen"
inEnglish 15 = Just "fifteen"
inEnglish 16 = Just "sixteen"
inEnglish 17 = Just "seventeen"
inEnglish 18 = Just "eighteen"
inEnglish 19 = Just "nineteen"
inEnglish 20 = Just "twenty"
inEnglish n
  | n < 0 = Nothing
  | n < 30 = ("twenty-" ++) <$> inEnglish (n `mod` 10)
  | n < 40 = ("thirty-" ++) <$> inEnglish (n `mod` 10)
  | n < 50 = ("forty-" ++) <$> inEnglish (n `mod` 10)
  | n < 60 = ("fifty-" ++) <$> inEnglish (n `mod` 10)
  | n < 70 = ("sixty-" ++) <$> inEnglish (n `mod` 10)
  | n < 80 = ("seventy-" ++) <$> inEnglish (n `mod` 10)
  | n < 90 = ("eighty-" ++) <$> inEnglish (n `mod` 10)
  | n < 100 = ("ninety-" ++) <$> inEnglish (n `mod` 10)
  | n == 100 = Just "one hundred"
  | n < 1000 = inEnglish (n `div` 100) +++ Just " hundred " +++ inEnglish (n `mod` 100)
  | n == 1000 = Just "one thousand"
  | n < 1000000 = inEnglish (n `div` 1000) +++ Just " thousand " +++ inEnglish (n `mod` 1000)
  | n == 1000000 = Just "one million"
  | n < 1000000000 = inEnglish (n `div` 1000000) +++ Just " million " +++ inEnglish (n `mod` 1000000)
  | n == 1000000000 = Just "one billion"
  | n < 1000000000000 = inEnglish (n `div` 1000000000) +++ Just " billion " +++ inEnglish (n `mod` 1000000000)
  | otherwise = Nothing
  where
    (+++) = liftM2 (++) :: Maybe String -> Maybe String -> Maybe String
