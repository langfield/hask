module Alphametics (solve) where

import Data.List ((\\))

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as MB

import Debug.Trace (trace)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

-- | Find every map which solves `terms`.
--
-- A map is an assignment of digits to characters.
solve :: String -> Maybe [(Char, Int)]
solve puzzle = L.find (`solves` terms) maps
  where
    maps     = map (zip ls') $ filter (zerosAfter l) $ L.permutations [0 .. 9]
    l        = length initials
    ls'      = initials ++ (letters \\ initials)
    initials = trace' "initials" $ L.nub $ map head terms
    letters  = L.nub $ concat terms
    terms    = trace' "terms" $ takeWhile (not . null) $ L.unfoldr f puzzle
      where
        f cs = return (term, rest')
          where
            (term, rest ) = span C.isUpper cs
            (_   , rest') = break C.isUpper rest

zerosAfter :: Int -> [Int] -> Bool
zerosAfter l p = 0 `notElem` trace' "permutation prefix" (take l (trace' "permutation" p))

solves :: [(Char, Int)] -> [String] -> Bool
solves cmap terms = sum (toInt <$> init terms) == toInt (last terms)
  where
    toInt xs = fromDigits $ MB.fromJust . (`lookup` cmap) <$> xs
    fromDigits = L.foldl' (\n x -> n * 10 + x) 0
