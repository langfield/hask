module Series (Error(..), largestProduct) where

import Text.Read (readMaybe)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

type Product = Integer
data State = State
  { stWindow :: [Integer]
  , stMaxProd :: Product
  }
  deriving Show

largestProduct :: Int -> String -> Either Error Integer
largestProduct 0 _  = Right 1
largestProduct _ "" = Left InvalidSpan
largestProduct size ds
  | size < 0  = Left InvalidSpan
  | size > length ds = Left InvalidSpan
  | otherwise = fmap stMaxProd $ foldr go (State [] 0) <$> mapM parse ds
  where
    parse :: Char -> Either Error Integer
    parse c = case readMaybe [c] of
      Nothing -> Left (InvalidDigit c)
      Just k  -> Right k

    go :: Integer -> State -> State
    go k (State xs maxProd)
      | k == 0 = State [] maxProd
      | length xs + 1 < size = State (k:xs) maxProd
      | length xs + 1 == size = State (k:xs) (max maxProd (product (k:xs)))
      | otherwise = State (k:init xs) (max maxProd (product (k:init xs)))
