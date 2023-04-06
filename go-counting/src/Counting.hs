module Counting (Color(..), territories, territoryFor) where
import Control.Arrow (first, second)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S

data Color = Black | White
    deriving (Eq, Ord, Show)

type X = Int
type Y = Int
type Coord = (X, Y)

-- A board is a set of empty coordinates and a map from coordinates to colors.
type Board = (Set Coord, Map Coord Color)
type Territory = (Set Coord, Maybe Color)

-- | Construct a board from the list-of-strings representation.
makeBoard :: [[Char]] -> Board
makeBoard css = foldr f (S.empty, M.empty) $ concat $ zipWith go [1 ..] css
  where
    -- Combine a y-coordinate with a row from the board.
    go :: Y -> [Char] -> [(X, Y, Char)]
    go y = zipWith (\x c -> (x, y, c)) [1 ..]
    -- Note that `first` just transforms functions `b -> c` to
    -- functions `(b, d) -> (c, d)` (and vice-versa for `second).
    --
    -- So basically `fmap : Just : Maybe` is analogous to `first : a : (a, b)`.
    f :: (X, Y, Char) -> Board -> Board
    f (x, y, 'W') = second (M.insert (x, y) White)
    f (x, y, 'B') = second (M.insert (x, y) Black)
    f (x, y, _  ) = first (S.insert (x, y))

-- | Find a territory, its color, and the resulting board with these points no
-- longer counted as empty, starting from some coordinate.
--
-- The starting coordinate must be empty (uncolored). If it is not, then we
-- return `Nothing`.
findTerritory :: Maybe Coord -> Board -> Maybe (Territory, Board)
findTerritory mbStart (empties, colored)
  | S.null empties           = Nothing
  | start `S.member` empties = Just ((area, color), board)
  | otherwise                = Nothing
  where
    -- If we are not told where to start, take the first empty coordinate.
    start         = fromMaybe (head $ S.toList empties) mbStart
    (area, board) = expandArea (S.singleton start) (S.delete start empties, colored)
    color         = checkPerimeter area colored

-- | Given an area (a set of coordinates) and a board, expand that area until
-- there are no empty coordinates adjacent to it.
--
-- We do this by finding all empty coordinates adjacent to points in `area`,
-- and checking if this set is empty (our termination condition). If not, then
-- we recurse, adding all the empty neighbors to `area` and reducing the set of
-- empty coordinates to those empty coordinates with no neighbors in the
-- previous `area`.
--
-- NOTE: We do *not* recolor points within this function. The color map of the
-- input board should be the same as the color map of the output board.
expandArea :: Set Coord -> Board -> (Set Coord, Board)
expandArea area board@(empties, colored)
  | S.null around = (area, board)
  | otherwise     = expandArea (S.union area around) (empties', colored)
  where
    -- Check if any of a point's neighbors are contained in `area`.
    isInArea :: Coord -> Bool
    isInArea coord = any ((`S.member` area) . ($ coord)) neighbours
    -- Partition `empties` into two disjoint subsets: all the points with
    -- neighbors in `area`, and all the rest.
    (around, empties') = S.partition isInArea empties

-- | Check the color of a given area (set of coordinates) against some mapping
-- of coordinates to colors.
--
-- We convert the set of coordinates to a list, and compute all neighbors of
-- each point. Then we lookup the color of each neighbor. The end result is the
-- set of colors of all boundary points of the area.
--
-- If this is just a single color, we return that color, otherwise, we return
-- nothing.
checkPerimeter :: Set Coord -> Map Coord Color -> Maybe Color
checkPerimeter area colored = case S.toList around of
  [color] -> Just color
  _       -> Nothing
  where
    around = S.fromList $ do
      a <- S.toList area
      b <- map ($ a) neighbours
      maybeToList $ M.lookup b colored

-- | Functions which return neighbors of a given coordinate.
neighbours :: [Coord -> Coord]
neighbours = [first succ, first pred, second succ, second pred]

gatherTerritories :: [Territory] -> Board -> [Territory]
gatherTerritories xs = maybe xs go . findTerritory Nothing
  where
    go :: (Territory, Board) -> [Territory]
    go (x, board) = gatherTerritories (x : xs) board

territories :: [[Char]] -> [Territory]
territories = gatherTerritories [] . makeBoard

territoryFor :: [[Char]] -> Coord -> Maybe Territory
territoryFor text start = fmap fst $ findTerritory (Just start) $ makeBoard text
