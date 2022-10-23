module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop
  | (start < 1) || (stop < 1) || (start > 12) || (stop > 12) || (start > stop) = []
  | otherwise = sentence start : recite (start + 1) stop


sentence :: Int -> String
sentence n
  | n <= 0 || n > 12 = ""
  | otherwise = "On the " ++ ordinal n ++ " day of Christmas my true love gave to me: " ++ giftPhrase n ++ "."


giftPhrase :: Int -> String
giftPhrase n =
  case gifts of
    []  -> ""
    [x] -> x
    xs  -> intercalate ", " ( init xs) ++ ", and " ++ last xs
  where gifts = giftSequence n


giftSequence :: Int -> [String]
giftSequence n
  | n <= 0 || n > 12 = []
  | n == 1 = [gift 1]
  |otherwise = gift n : giftSequence (n - 1)


ordinal :: Int -> String
ordinal n =
  case n of
    1 -> "first"
    2 -> "second"
    3 -> "third"
    4 -> "fourth"
    5 -> "fifth"
    6 -> "sixth"
    7 -> "seventh"
    8 -> "eighth"
    9 -> "ninth"
    10 -> "tenth"
    11 -> "eleventh"
    12 -> "twelfth"
    _ -> ""


gift :: Int -> String
gift n =
  case n of
    1 -> "a Partridge in a Pear Tree"
    2 -> "two Turtle Doves"
    3 -> "three French Hens"
    4 -> "four Calling Birds"
    5 -> "five Gold Rings"
    6 -> "six Geese-a-Laying"
    7 -> "seven Swans-a-Swimming"
    8 -> "eight Maids-a-Milking"
    9 -> "nine Ladies Dancing"
    10 -> "ten Lords-a-Leaping"
    11 -> "eleven Pipers Piping"
    12 -> "twelve Drummers Drumming"
    _ -> ""
