module Yacht (yacht, Category(..)) where

import Data.Map (Map)
import Data.List (sort, sortOn)
import qualified Data.Map as M

type DieValue = Int

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

infixl 9 !

(!) :: Ord k => Map k Int -> k -> Int
m ! k = M.findWithDefault 0 k m

yacht :: Category -> [Int] -> Int
yacht Choice dice = sum dice
yacht LittleStraight dice = if sort dice == [1, 2, 3, 4, 5] then 30 else 0
yacht BigStraight dice = if sort dice == [2, 3, 4, 5, 6] then 30 else 0
yacht cat dice =
  case cat of
    Ones -> counts ! 1
    Twos -> 2 * counts ! 2
    Threes -> 3 * counts ! 3
    Fours -> 4 * counts ! 4
    Fives -> 5 * counts ! 5
    Sixes -> 6 * counts ! 6
    FullHouse -> if a == b && b /= c && c == d && d == e then 2 * a + 3 * c else 0
    FourOfAKind -> if b == c && c == d && d == e then 4 * b else 0
    Yacht -> if a == b && b == c && c == d && d == e then 50 else 0
  where
    counts = getCounts dice
    [a, b, c, d, e] = sortByFrequency counts dice

getCounts :: [DieValue] -> Map DieValue Int
getCounts xs = M.fromListWith (+) $ zip xs $ repeat 1

sortByFrequency :: Map DieValue Int -> [DieValue] -> [DieValue]
sortByFrequency counts xs = sortOn (\x -> counts ! x) xs
