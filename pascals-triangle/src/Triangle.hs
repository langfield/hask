module Triangle (rows) where

import Data.List (tails)

rows :: Int -> [[Integer]]
rows x = buildTriangle x []


-- Build a triangle with `n` rows. Given a bound and some rows so far, it takes
-- the last row, computes the successor row, and returns the concatenation.
buildTriangle :: Int -> [[Integer]] -> [[Integer]]
buildTriangle n [] = buildTriangle n [[1]]
buildTriangle n xss
  | n <= 0 = []
  | n == 1 = [[1]]
  | n <= length xss = xss
  | otherwise = buildTriangle n $ xss ++ [(nextRow . last) xss]


-- I'm not even sure why this works, I just kept composing shit until something stuck.
nextRow :: [Integer] -> [Integer]
nextRow xs = 1 : (init . map (sum . take 2)) (tails xs)
