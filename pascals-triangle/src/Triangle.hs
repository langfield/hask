module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = take x triangle

-- To obtain the successor row, we take two copies of xs, shift one of them to
-- the right by prepending a zero, and then sum pairs of elements in the same
-- position. This is equivalent to summing up pairs of adjacent elements in the
-- original row. The `1` is added to make the row symmetric.
triangle :: [[Integer]]
triangle = iterate next [1]
  where
    next :: [Integer] -> [Integer]
    next xs = tail $ zipWith (+) (cycle ys) (0 : ys)
      where ys = 0 : xs
