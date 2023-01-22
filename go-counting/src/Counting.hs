module Counting (Color(..), territories, territoryFor) where
import Control.Arrow (first, second)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S

data Color = Black | White
    deriving (Eq, Ord, Show)

type X = Int
type Y = Int
type Coord = (X, Y)

-- A board is a set of coordinates and a map from coordinates to color.
type Board = (S.Set Coord, M.Map Coord Color)

makeBoard :: [[Char]] -> Board
makeBoard css = foldr f (S.empty, M.empty) $ concat $ zipWith go [1 ..] css
  where
    -- Combine a y-coordinate with a row from the board.
    go :: Y -> [Char] -> [(Y, (X, Char))]
    go y = zipWith (\x c -> (y, (x, c))) [1 ..]

    f :: (Y, (X, Char)) -> Board -> Board
    f (y, (x, 'W')) = second (M.insert (x, y) White)
    f (y, (x, 'B')) = second (M.insert (x, y) Black)
    f (y, (x, _)  ) = first (S.insert (x, y))

findTerritory :: Maybe Coord
              -> Board
              -> Maybe ((S.Set Coord, Maybe Color), Board)
findTerritory mstart (empties, colored)
  | S.null empties = Nothing
  | start `S.notMember` empties = Nothing
  | otherwise = Just ((area, color), board)
  where
    start = fromMaybe (head $ S.toList empties) mstart
    (area, board) =
      expandArea (S.singleton start) (S.delete start empties, colored)
    color = checkPerimeter area colored

expandArea :: S.Set Coord -> Board -> (S.Set Coord, Board)
expandArea area board@(empties, colored)
  | S.null around = (area, board)
  | otherwise     = expandArea (S.union area around) (empties', colored)
  where
    (around, empties') = S.partition f empties
      where f a = any ((`S.member` area) . ($ a)) neighbours

checkPerimeter :: S.Set Coord -> M.Map Coord Color -> Maybe Color
checkPerimeter area colored = case S.toList around of
  [color] -> Just color
  _ -> Nothing
  where
    around = S.fromList $ do
      a <- S.toList area
      b <- map ($ a) neighbours
      maybeToList $ M.lookup b colored

neighbours :: [Coord -> Coord]
neighbours = [first succ, first pred, second succ, second pred]

territories :: [[Char]] -> [(S.Set Coord, Maybe Color)]
territories = f [] . makeBoard
  where f xs = maybe xs (uncurry (\x -> f (x : xs))) . findTerritory Nothing

territoryFor :: [[Char]] -> Coord -> Maybe (S.Set Coord, Maybe Color)
territoryFor text start =
  fmap fst $ findTerritory (Just start) $ makeBoard text
