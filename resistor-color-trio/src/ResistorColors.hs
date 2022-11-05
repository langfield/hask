module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor = labelForOhms $ ohms resistor

labelForOhms :: Int -> String
labelForOhms r
  | r < (10 ^ 3) = show r ++ " ohms"
  | r < (10 ^ 6) = show (r `div` 10^3) ++ " kiloohms"
  | r < (10 ^ 9) = show (r `div` 10^6) ++ " megaohms"
  | otherwise = show (r `div` 10^9) ++ " gigaohms"

ohms :: Resistor -> Int
ohms (Resistor (a, b, c)) = ((tens * 10) + ones) * (10 ^ zeros)
  where
    tens = bandResistance a
    ones = bandResistance b
    zeros = bandResistance c

bandResistance :: Color -> Int
bandResistance Black = 0
bandResistance Brown = 1
bandResistance Red = 2
bandResistance Orange = 3
bandResistance Yellow = 4
bandResistance Green = 5
bandResistance Blue = 6
bandResistance Violet = 7
bandResistance Grey = 8
bandResistance White = 9
