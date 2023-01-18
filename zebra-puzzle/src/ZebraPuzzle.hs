module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad (guard)
import Data.List (permutations, zip5)

data Color = Red | Green | Ivory | Yellow | Blue deriving (Eq, Show)

data Beverage = Coffee | Tea | Milk | OrangeJuice | Water deriving (Eq, Show)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments deriving (Eq, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra deriving (Eq, Show)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  }
  deriving (Eq, Show)


solutions :: [[(Color, Resident, Pet, Beverage, Cigarette)]]
solutions = do
  nationalities <- permutations
    [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
  guard $ head nationalities == Norwegian -- 10
  colors <- permutations [Red, Green, Ivory, Yellow, Blue]
  guard $ (Ivory, Green) `elem` zip colors (tail colors) -- 6
  guard $ (Englishman, Red) `elem` zip nationalities colors -- 2
  beverages <- permutations [Coffee, Tea, Milk, OrangeJuice, Water]
  guard $ head (tail $ tail beverages) == Milk -- 9
  guard $ (Coffee, Green) `elem` zip beverages colors -- 4
  guard $ (Ukrainian, Tea) `elem` zip nationalities beverages -- 5
  cigarettes <- permutations
    [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
  guard $ (Kools, Yellow) `elem` zip cigarettes colors -- 8
  pets <- permutations [Dog, Snails, Fox, Horse, Zebra]
  guard $ (Spaniard, Dog) `elem` zip nationalities pets -- 3
  guard $ (OldGold, Snails) `elem` zip cigarettes pets -- 7
  guard
    $  (Chesterfields, Fox)
    `elem` zip cigarettes (tail pets)
    || (Fox, Chesterfields)
    `elem` zip pets (tail cigarettes) -- 11
  guard
    $  (Kools, Horse)
    `elem` zip cigarettes (tail pets)
    || (Horse, Kools)
    `elem` zip pets (tail cigarettes) -- 12
  guard $ (LuckyStrike, OrangeJuice) `elem` zip cigarettes beverages -- 13
  guard $ (Japanese, Parliaments) `elem` zip nationalities cigarettes -- 14
  guard
    $  (Norwegian, Blue)
    `elem` zip nationalities (tail colors)
    || (Blue, Norwegian)
    `elem` zip colors (tail nationalities) -- 15
  pure $ zip5 colors nationalities pets beverages cigarettes


solve :: Solution
solve = Solution whoDrinksWater whoOwnsZebra
  where
    whoDrinksWater = head [ resident | (_, resident, _, Water, _) <- solution ]
    whoOwnsZebra = head [ resident | (_, resident, Zebra, _, _) <- solution ]
    solution     = head solutions

