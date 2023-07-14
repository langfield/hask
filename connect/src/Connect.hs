module Connect (Mark(..), winner) where

import qualified Data.List as L

data Mark = Cross | Nought deriving (Eq, Show)

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
-- This should probably be a simple DFS function, right?
won :: Char -> [String] -> Bool
won c [] = True
won c (r : rs) = False
