module ResistorColors (Color(..), value) where

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
  deriving (Eq, Show, Enum, Bounded)

value :: (Color, Color) -> Int
value (a, b) = (colorVal a) * 10 + colorVal b


colorVal :: Color -> Int
colorVal Black = 0
colorVal Brown = 1
colorVal Red = 2
colorVal Orange = 3
colorVal Yellow = 4
colorVal Green = 5
colorVal Blue = 6
colorVal Violet = 7
colorVal Grey = 8
colorVal White = 9
