module Alphametics (solve) where

import Data.List ((\\))

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as MB

solve :: String -> Maybe [(Char, Int)]
solve puzzle = L.find (`solves` terms) maps
  where
    maps     = map (zip ls') $ filter (zerosAfter l) $ L.permutations [0 .. 9]
    l        = length initials
    ls'      = initials ++ (letters \\ initials)
    initials = L.nub $ map head terms
    letters  = L.nub $ concat terms
    terms    = takeWhile (not . null) $ L.unfoldr f puzzle
      where
        f cs = return (term, rest')
          where
            (term, rest ) = span C.isUpper cs
            (_   , rest') = break C.isUpper rest

zerosAfter :: Int -> [Int] -> Bool
zerosAfter l p = 0 `notElem` take l p

solves :: [(Char, Int)] -> [String] -> Bool
solves cmap terms = sum (toInt <$> init terms) == toInt (last terms)
  where
    toInt xs = fromDigits $ MB.fromJust . (`lookup` cmap) <$> xs
    fromDigits = L.foldl' (\n x -> n * 10 + x) 0
