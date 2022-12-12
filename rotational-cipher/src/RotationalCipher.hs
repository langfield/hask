module RotationalCipher (rotate) where

import qualified Data.Char as C

rotate :: Int -> String -> String
rotate m s = map (go m) s
  where
    shift :: Int -> Int -> Int -> Int
    shift n offset ord = (ord + n - offset) `mod` 26 + offset

    go :: Int -> Char -> Char
    go n c
      | not $ C.isLetter c = c
      | C.isAlpha c        = C.chr $ shift n (C.ord 'A') (C.ord c)
      | otherwise          = C.chr $ shift n (C.ord 'a') (C.ord c)
