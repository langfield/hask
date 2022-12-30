{-# LANGUAGE OverloadedStrings #-}
module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Control.Monad (replicateM)
import qualified Data.Char as C
import qualified System.Random as R

caesarDecode :: String -> String -> String
caesarDecode [k] cs = map (shiftL k) cs
caesarDecode _   "" = ""
caesarDecode ks  cs = go ks cs
  where
    go :: String -> String -> String
    go "" text     = text
    go _  ""       = ""
    go (x : xs) (y : ys) = shiftL x y : go xs ys

shiftL :: Char -> Char -> Char
shiftL k c = C.chr $ ((C.ord c - (C.ord k - 97) - 97) `mod` 26) + 97

shiftR :: Char -> Char -> Char
shiftR k c = C.chr $ ((C.ord c + (C.ord k - 97) - 97) `mod` 26) + 97

caesarEncode :: String -> String -> String
caesarEncode [k] cs = map (shiftR k) cs
caesarEncode _   "" = ""
caesarEncode ks  cs = go ks cs
  where
    go :: String -> String -> String
    go "" text     = text
    go _  ""       = ""
    go (x : xs) (y : ys) = shiftR x y : go xs ys

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomString 100
  pure $ (key, caesarEncode key text)
  where
    randomString :: Int -> IO String
    randomString len = replicateM len $ R.randomRIO ('a', 'z')
