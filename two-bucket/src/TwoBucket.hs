module TwoBucket (measure) where

import Data.Maybe(listToMaybe)
import Data.List(sortBy)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capx, capy) target = 
  listToMaybe $ sortBy (\(a,_) (b,_) -> compare a b) $ go [(0, 0)]
  where
    go :: [(Int, Int)] -> [(Int, (Int, Int))]
    go [] = []
    go path@((x, y) : rest)
      | x == 0 && y == capy = []
      | x > capx = []
      | y > capy = []
      | x < 0 = []
      | y < 0 = []
      | (x == target) || (y == target) = [(length rest, (x, y))]
      | (x, y) `elem` rest = []
      | otherwise =
            concat 
              [ go $ (0, x + y):path
              , go $ (x + y - capy, capy):path
              , go $ (x + y, 0):path
              , go $ (capx, x + y - capx):path
              , go $ (x, 0):path
              , go $ (0, y):path
              , go $ (capx, y):path
              , go $ (x, capy):path
              ]
