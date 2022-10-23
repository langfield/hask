module Queens (boardString, canAttack) where

import qualified Data.Ix as I
import qualified Data.Text as T
import qualified Data.Array as A

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = displayBoard $ (place 'W' white . place 'B' black) board
  where board = A.listArray ((0, 0), (7, 7)) $ replicate 64 '_'

-- Place a queen with a given marker, location on a board.
place :: Char -> Maybe (Int, Int) -> A.Array (Int, Int) Char -> A.Array (Int, Int) Char
place _ Nothing board = board
place c (Just (x, y)) board
  | I.inRange ((0, 0), (7, 7)) (x, y) = board A.// [((x, y), c)]
  | otherwise = board

-- Convert a board to a string.
displayBoard :: A.Array (Int, Int) Char -> String
displayBoard board = T.unpack $ T.unlines $ map (T.intersperse ' ') $ unflatten $ A.elems board

-- Convert from a list of 64 chars to a length 8 list of length 8 text strings.
unflatten :: [Char] -> [T.Text]
unflatten [] = []
unflatten xs = T.pack (take 8 xs) : unflatten (drop 8 xs)

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) = x1 == x2 || y1 == y2 || abs (y2 - y1) == abs(x2 - x1)
