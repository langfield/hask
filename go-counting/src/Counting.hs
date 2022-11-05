module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import Data.Array (Array, (!))
import qualified Data.Set as S
import qualified Data.Array as A

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type BoardArray = Array Coord Char

-- Search for homgeneous territories from each empty space.
arrayTerritories :: BoardArray -> Set Coord -> [(Set Coord, Maybe Color)]
arrayTerritories arr spcs = foldr (search arr) [] spcs

search :: BoardArray -> Coord -> [(Set Coord, Maybe Color)] -> [(Set Coord, Maybe Color)]
search arr spc results

territories :: [String] -> [(Set Coord, Maybe Color)]
territories [] = []
territories [ "B" ] = []
territories [ " " ] = [(S.singleton (1, 1), Nothing)]
territories board = []
  where
    m = length board
    n = length $ head board
    arr = A.listArray ((1, 1), (m, n)) (concat board)
    spcs = S.fromList $ filter (\ix -> arr ! ix == ' ') $ A.indices arr

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = Nothing
