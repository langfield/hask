module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode s =
  case span isDigit s of
    (_, "") -> ""
    ("", c:cs) -> c : decode cs
    (n, c:cs) -> replicate (read n :: Int) c ++ decode cs

encode :: String -> String
encode "" = ""
encode (c:cs) = go $ span (== c) cs
  where
    go ("", suffix) = c : encode suffix
    go (prefix, suffix) = show (1 + length prefix) ++ (c : encode suffix)
