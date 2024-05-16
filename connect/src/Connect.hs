module Connect (Mark(..), winner) where

-- We map each element in the 2D array to a list of its 6 neighbors.

data Mark = Cross | Nought deriving (Eq, Show)

unstagger :: [[a]] -> [[a]]
unstagger = zipWith drop [0..]

augment :: a -> [[a]] -> [[a]]
augment d m = map (pad d) . pad (replicate ncols d) $ m
  where
    pad d' = reverse . (d' :) . reverse . (d' :)
    ncols = foldr (max . length) 0 m

neighbors :: a -> [[a]] -> [[[a]]]
neighbors = undefined

-- | Compute the Mark of the winner of a given board.
winner :: [String] -> Maybe Mark
winner = undefined . map (filter (== ' ')) .  unstagger
