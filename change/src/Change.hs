module Change (findFewestCoins) where

import Data.List (sort, sortBy)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | otherwise =
    case sols of
      [] -> Nothing
      (x : _) -> Just x
    where
      sols = sortByLength $ getSolutions target $ reverse $ sort coins

lengthOrdering :: [a] -> [a] -> Ordering
lengthOrdering xs ys
  | length xs == length ys = EQ
  | length xs < length ys = LT
  | otherwise = GT

sortByLength :: [[Integer]] -> [[Integer]]
sortByLength = sortBy lengthOrdering

getSolutions :: Integer -> [Integer] -> [[Integer]]
getSolutions 0 _ = [[]]
getSolutions _ [] = []
getSolutions target (x : xs)
  | x > target = getSolutions target xs
  | otherwise = getCandidates target x xs maxCount ++ getSolutions target xs
  where 
    maxCount = fromIntegral $ target `div` x :: Int

getCandidates :: Integer -> Integer -> [Integer] -> Int -> [[Integer]]
getCandidates target _ rest 0 = getSolutions target rest
getCandidates target coin rest maxCount =
  case map (copies ++) (getSolutions (target - sum copies) rest) of
    [] -> getCandidates target coin rest (maxCount - 1)
    xs -> xs
  where
    copies = replicate maxCount coin
