module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance s t = go s t (Just 0)
  where
    go :: String -> String -> Maybe Int -> Maybe Int
    go "" "" acc = acc
    go (x:xs) (y:ys) acc = go xs ys (if x == y then acc else (+1) <$> acc)
    go _ _ _ = Nothing
