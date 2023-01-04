module RailFenceCipher (encode, decode) where

import qualified Data.List as L

data Direction = Up | Down deriving (Eq, Show)

mkCols :: Int -> Int -> Direction -> String -> [String]
mkCols _ _ _ "" = []
mkCols n i dir (c : cs)
  | n <= 0    = []
  | i >= n    = [replicate n '.']
  | dir == Down && i == end = col : mkCols n (max 0 (end - 1)) Up cs
  | dir == Up && i == 0 = col : mkCols n (min (i + 1) end) Down cs
  | dir == Down = col : mkCols n ((i + 1) `mod` n) Down cs
  | dir == Up = col : mkCols n ((i - 1) `mod` n) Up cs
  | otherwise = [replicate n '.']
  where
    end    = n - 1
    prefix = replicate i '.'
    suffix = replicate (n - i - 1) '.'
    col    = prefix ++ [c] ++ suffix

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encode :: Int -> String -> String
encode n s
  | n <= 0    = ""
  | otherwise = (filter (/= '.') . L.concat . L.transpose . mkCols n 0 Down) s

type Row = Int
type Col = Int
type Source = String
type Result = String

decode :: Int -> String -> String
decode n s
  | n <= 0
  = ""
  | otherwise
  = filter (/= '.')
    $ L.concat
    $ L.transpose
    $ chunksOf (length s)
    $ reverse
    $ snd
    $ L.foldl'
        go
        (s, "")
        [ (i, j) | i <- [0 .. n - 1], j <- [0 .. length s - 1] ]
  where
    go :: (Source, Result) -> (Row, Col) -> (Source, Result)
    go ("", res) _ = ("", '.' : res)
    go (c : cs, res) (i, j)
      | isZig n (i, j) = (cs, c : res)
      | isZag n (i, j) = (cs, c : res)
      | otherwise = (c : cs, '.' : res)

-- Determines whether we are on the downward diagonal of the zig-zag.
isZig :: Int -> (Int, Int) -> Bool
isZig n (i, j)
  | n <= 0 || i < 0 || j < 0 || i >= n = False
  | otherwise = j `mod` period == i
  where period = 2 * (n - 1)

isZag :: Int -> (Int, Int) -> Bool
isZag n (i, j)
  | n <= 0 || i < 0 || j < 0 || i >= n = False
  | otherwise = j `mod` period == (-i) `mod` period
  where period = 2 * (n - 1)

-- e . . . . . . . i . . . . . . . e
-- . x . . . . . m . s . . . . . m
-- . . e . . . s . . . a . . . o
-- . . . r . i . . . . . w . s
-- . . . . c . . . . . . . e
