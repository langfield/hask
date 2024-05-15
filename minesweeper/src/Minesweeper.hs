module Minesweeper where

import Data.Char (intToDigit)
import Data.List (transpose)

-- | Add a ring of default elements around a 2d array.
augment :: a -> [[a]] -> [[a]]
augment d m = map (pad d) . pad (replicate ncols d) $ m
  where
    pad d' = reverse . (d' :) . reverse . (d' :)
    ncols = foldr (max . length) 0 m

-- | Neighbors in a 2d array, with default.
neighbors :: a -> [[a]] -> [[[a]]]
neighbors d = map transpose . transpose . map' [right, up, left, down, downR, upR, upL, downL] . augment d
  where
    map' fs x = map ($ x) fs
    rot   = transpose . reverse
    right = map (drop 2) . reverse . drop 1 . reverse . drop 1
    up    = rot . rot . rot . right . rot
    left  = rot . rot . right . rot . rot
    down  = rot . right . rot . rot . rot
    downR = map (drop 2) . drop 2
    upR   = rot . rot . rot . downR . rot
    upL   = rot . rot . downR . rot . rot
    downL = rot . downR . rot . rot . rot

count :: [Char] -> Char
count = intToDigit . length . filter (== '*')

format :: Char -> [Char] -> Char
format '*' _ = '*'
format _ nbs = mkspaces (count nbs)

mkspaces :: Char -> Char
mkspaces '0' = ' '
mkspaces c = c

annotate :: [[Char]] -> [[Char]]
annotate m = zipWith (zipWith format) m (neighbors ' ' m)
