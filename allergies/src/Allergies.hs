module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show)

allergies :: Int -> [Allergen]
allergies x = concat
  [ if isAllergicTo Eggs x then [Eggs] else []
  , if isAllergicTo Peanuts x then [Peanuts] else []
  , if isAllergicTo Shellfish x then [Shellfish] else []
  , if isAllergicTo Strawberries x then [Strawberries] else []
  , if isAllergicTo Tomatoes x then [Tomatoes] else []
  , if isAllergicTo Chocolate x then [Chocolate] else []
  , if isAllergicTo Pollen x then [Pollen] else []
  , if isAllergicTo Cats x then [Cats] else []
  ]


isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo Eggs         x = x `mod` 2 >= 1
isAllergicTo Peanuts      x = x `mod` 4 >= 2
isAllergicTo Shellfish    x = x `mod` 8 >= 4
isAllergicTo Strawberries x = x `mod` 16 >= 8
isAllergicTo Tomatoes     x = x `mod` 32 >= 16
isAllergicTo Chocolate    x = x `mod` 64 >= 32
isAllergicTo Pollen       x = x `mod` 128 >= 64
isAllergicTo Cats         x = x `mod` 256 >= 128


  {-
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a n = testBit n (fromEnum a)
        
allergies :: Int -> [Allergen]
allergies n = filter (`isAllergicTo` n) [Eggs .. Cats]
-}
