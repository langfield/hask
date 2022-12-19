module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Array (Array, (!))
import Data.Vector (Vector)

import qualified Data.Array as A
import qualified Data.Vector as V

data Matrix a = Matrix (Array (Int, Int) a) | Empty
  deriving (Eq, Show)

cols :: Matrix a -> Int
cols = snd . shape

column :: Int -> Matrix a -> Vector a
column _ Empty = V.empty
column y (Matrix arr) = V.fromList [arr ! (x, y) | x <- [1..m]]
  where
    m = rows (Matrix arr)

flatten :: Matrix a -> Vector a
flatten Empty = V.fromList []
flatten (Matrix arr) = (V.fromList . A.elems) arr

fromList :: [[a]] -> Matrix a
fromList [] = Empty
fromList xss@(xs:_) = Matrix $ A.listArray ((1, 1), (m, n)) (concat xss)
  where
    m = length xss
    n = length xs

fromString :: (Read a, Show a) => String -> Matrix a
fromString s = fromList xs
  where
    xs = (map (map read) . map words . lines) s

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs) 

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape _ Empty = Empty
reshape (_, n') (Matrix arr) = (fromList . chunksOf n' . A.elems) arr

row :: Int -> Matrix a -> Vector a
row _ Empty = V.empty
row x (Matrix arr) = V.fromList [arr ! (x, y) | y <- [1..n]]
  where
    n = cols (Matrix arr)

rows :: Matrix a -> Int
rows = fst . shape

shape :: Matrix a -> (Int, Int)
shape Empty = (0, 0)
shape (Matrix arr) = (m, n)
  where
    (_, (m, n)) = A.bounds arr

transpose :: Matrix a -> Matrix a
transpose Empty = Empty
transpose (Matrix arr) = Matrix $ A.ixmap ((1, 1), (n, m)) (\(i, j) -> (j, i)) arr
  where
    (_, (m, n)) = A.bounds arr
