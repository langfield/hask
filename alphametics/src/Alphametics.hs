module Alphametics (solve) where

import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as MB

solve' :: NonEmpty String -> Maybe [(Char, Int)]
solve' terms = L.find (`solves` terms) . map (zip letters) . filter (noZerosPriorTo n) . L.permutations $ [0 .. 9]
  where
    initials    = L.nub . MB.catMaybes . NE.toList . NE.map MB.listToMaybe $ terms
    nonInitials = L.nub (concat terms) \\ initials
    letters     = initials ++ nonInitials
    n           = length initials

noZerosPriorTo :: Int -> [Int] -> Bool
noZerosPriorTo n p = 0 `notElem` take n p

solve :: String -> Maybe [(Char, Int)]
solve puzzle = solve' =<< NE.nonEmpty terms
  where
    terms = filter (all C.isUpper) . words $ puzzle

solves :: [(Char, Int)] -> NonEmpty String -> Bool
solves cmap terms = (sum <$> mapM termToInt (NE.init terms)) == termToInt (NE.last terms)
  where
    termToInt :: [Char] -> Maybe Int
    termToInt xs = fromDigits <$> mapM (`lookup` cmap) xs

    fromDigits :: [Int] -> Int
    fromDigits = L.foldl' (\n x -> n * 10 + x) 0
