module BinarySearch (find) where

import Data.Array (Array, (!))
import qualified Data.Array as A

import Debug.Trace (trace)

ttt :: Show a => String -> a -> a
ttt s x = trace (s ++ ": " ++ show x) x

find :: (Show a, Ord a) => Array Int a -> a -> Maybe Int
find arr x
  | lb > ub        = Nothing
  | arr ! lb == x  = Just lb
  | arr ! ub == x  = Just ub
  | arr ! mid == x = Just mid
  | lb == ub       = Nothing
  | otherwise      =
    case find lows x of
      Just i -> Just i
      Nothing -> find highs x
  where
    (lb, ub) = A.bounds $ ttt "arr" arr
    mid = (lb + ub) `div` 2
    lows = A.array (lb, mid) $ filter ((<= mid) . fst) (A.assocs arr)
    highs = A.array (mid + 1, ub) $ filter ((> mid) . fst) (A.assocs arr)
