module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Control.Arrow (first, second)

import Data.Set (Set)
import Data.Map (Map)

import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace (trace)

traceAs :: Show a => String -> a -> a
traceAs msg x = trace (msg ++ ": " ++ show x) x

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

type Territory = (Set Coord, Maybe Color)

type Empties = Set Coord
type ColorMap = Map Coord Color

mkBoard :: [[Char]] -> (Empties, ColorMap)
mkBoard rows = foldr mkRow (S.empty, M.empty) $ zip rows [1..]
  where
    mkSquare :: (Char, (Int, Int)) -> (Empties, ColorMap) -> (Empties, ColorMap)
    mkSquare ('B', (i, j)) (empties, colors) = (empties, M.insert (i, j) Black colors)
    mkSquare ('W', (i, j)) (empties, colors) = (empties, M.insert (i, j) White colors)
    mkSquare (_, (i, j)) (empties, colors) = (S.insert (i, j) empties, colors)

    mkRow :: ([Char], Int) -> (Empties, ColorMap) -> (Empties, ColorMap)
    mkRow (xs, i) board = foldr mkSquare board $ zip xs (zip (repeat i) [1..])

-- When should this function return Nothing?
findTerritory :: Maybe Coord -> (Empties, ColorMap) -> Maybe (Territory, (Empties, ColorMap))
findTerritory Nothing _ = Nothing
findTerritory (Just coord) board@(empties, colors)
  | coord `S.member` empties = expandArea coord board
  | otherwise = Nothing

-- | Find the territory enclosing an empty coordinate, with those coordinates
-- removed from the `empties` set.
--
-- Invariant: `coord` must be contained in the `empties` set.
expandArea :: Coord -> (Empties, ColorMap) -> Maybe (Territory, (Empties, ColorMap))
expandArea coord (empties, colors) = Nothing
  where
    neighbors = map (\f -> f coord) neighborFuncs

neighborFuncs :: [Coord -> Coord]
neighborFuncs = [first (+1), first ((-) 1), second (+1), second ((-) 1)]

mkTerritories :: (Empties, ColorMap) -> [Territory]
mkTerritories (empties, colors) = []

territories :: [String] -> [Territory]
territories = mkTerritories . traceAs "board" . mkBoard

territoryFor :: [String] -> Coord -> Maybe Territory
territoryFor board coord = error "You need to implement this function."
