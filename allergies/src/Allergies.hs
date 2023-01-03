module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (testBit)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum)

-- | Number the constructors and then check if `n`th bit of argument is `1`.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a n = testBit n (fromEnum a)

allergies :: Int -> [Allergen]
allergies n = filter (`isAllergicTo` n) [Eggs .. Cats]
