module Alphametics (solve) where

import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as MB

-- | Find every map which solves `terms`.
--
-- A map is an assignment of digits to characters.
solve' :: NonEmpty String -> Maybe [(Char, Int)]
solve' terms = L.find (`solves` terms) maps
  where
    maps     = map (zip chars) $ filter (\p -> 0 `notElem` take (length initials) p) $ L.permutations [0 .. 9]
    chars    = initials ++ (L.nub (concat terms) \\ initials)
    initials = (L.nub . MB.catMaybes . NE.toList . NE.map MB.listToMaybe) terms

solve :: String -> Maybe [(Char, Int)]
solve puzzle = solve' =<< NE.nonEmpty (filter (all C.isUpper) $ words puzzle)

solves :: [(Char, Int)] -> NonEmpty String -> Bool
solves cmap terms = sum (toInt <$> NE.init terms) == toInt (NE.last terms)
  where
    toInt xs = fromDigits $ MB.fromJust . (`lookup` cmap) <$> xs
    fromDigits = L.foldl' (\n x -> n * 10 + x) 0
