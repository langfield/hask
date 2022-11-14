module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Map (Map)
import qualified Data.Map as M

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Child = String
type Garden = Map Child [Plant]

parsePlant :: Char -> Plant
parsePlant 'C' = Clover
parsePlant 'G' = Grass
parsePlant 'R' = Radishes
parsePlant 'V' = Violets

garden :: [String] -> String -> Garden
garden students plants =
  case (lines plants) of
    [xs, ys] -> M.fromList $ zip students plantSets
      where 
        xs' = chunksOf2 $ map parsePlant xs
        ys' = chunksOf2 $ map parsePlant ys
        plantSets = map (\(x, y) -> x ++ y) $ zip xs' ys'
    _ -> M.empty

chunksOf2 :: [a] -> [[a]]
chunksOf2 [] = []
chunksOf2 [_] = []
chunksOf2 (x : y : xs) = [x, y] : chunksOf2 xs

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student g =
  case M.lookup student g of
    Just xs -> xs
    Nothing -> []
