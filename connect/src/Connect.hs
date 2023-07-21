module Connect (Mark(..), winner) where

import qualified Data.List as L

data Mark = Cross | Nought deriving (Eq, Show)
type Board = [String]
type Player = Char

winner :: [String] -> Maybe Mark
winner board
  | cross = Just Cross
  | nought = Just Nought
  | otherwise = Nothing
  where
    cross = won 'X' board
    nought = won 'O' (L.transpose board)

-- | Check if `c` has connected top-to-bottom.
--
-- The easiest way to do this is just to return `True` when we've reached the
-- last row. This should probably be a simple DFS function, right?
won :: Player -> Board -> Bool
won c board = any $ map (uncurry (search c board))

search :: Player -> Board -> Int -> Int -> Bool
search c [] _ _ = True
search c rows x y = False
