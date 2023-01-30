module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import Data.Map (Map)

import qualified Data.Set as S
import qualified Data.Map as M

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

type Territory = (Set Coord, Maybe Color)

type Board = (Set Coord, Map Coord Color)

mkBoard :: [[Char]] -> Board
mkBoard rows = foldr mkRow (S.empty, M.empty) $ zip rows [1..]
  where
    mkSquare :: (Char, (Int, Int)) -> Board -> Board
    mkSquare ('B', (i, j)) (empties, colors) = (empties, M.insert (i, j) Black colors)
    mkSquare ('W', (i, j)) (empties, colors) = (empties, M.insert (i, j) White colors)
    mkSquare (_, (i, j)) (empties, colors) = (S.insert (i, j) empties, colors)

    mkRow :: ([Char], Int) -> Board -> Board
    mkRow (xs, i) board = foldr mkSquare board $ zip xs (zip (repeat i) [1..])

mkTerritories :: Board -> [Territory]
mkTerritories board = []

territories :: [String] -> [Territory]
territories = mkTerritories . mkBoard

territoryFor :: [String] -> Coord -> Maybe Territory
territoryFor board coord = error "You need to implement this function."
