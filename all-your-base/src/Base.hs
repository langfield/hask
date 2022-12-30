module Base (Error(..), rebase) where

import Data.Foldable (foldlM)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

toDecimal :: Integral a => a -> [a] -> Either (Error a) a
toDecimal base = foldlM (go base) 0
  where
    go :: Integral a => a -> a -> a -> Either (Error a) a
    go base' y x
      | x >= 0 && x < base' = Right (y * base' + x)
      | otherwise = Left (InvalidDigit x)

fromDecimal :: Integral a => a -> a -> [a]
fromDecimal _    0 = []
fromDecimal base y = y `mod` base : (fromDecimal base $ y `div` base)

rebase :: (Show a, Integral a) => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase <= 1 = Left InvalidInputBase
  | outputBase <= 1 = Left InvalidOutputBase
  | otherwise = reverse <$> fromDecimal outputBase <$> (toDecimal inputBase inputDigits)
