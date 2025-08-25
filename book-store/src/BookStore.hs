module BookStore (total, Book(..)) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

counts :: Ord a => [a] -> Map a Int
counts = foldr (\x -> M.insertWith (+) x 1) M.empty

-- Invariant: values are sorted, nonnegative.
price :: [Int] -> Int
price = go . filter (/= 0)
  where
    go [a] = 800*a
    go [a,b] = 760*2*b + price [a-b]
    go [a,b,c] = 720*3*c + price [a-c,b-c]
    go [a,b,c,d] = 640*4*d + price [a-d,b-d,c-d]
    go [a,b,c,d,e] = min
      (600*5*e + price [a-e,b-e,c-e,d-e])
      (640*4*d + price [a-d,b-d,c-d,e])
    go _ = 0

total :: [Book] -> Int
total = price . reverse . sort . filter (/= 0) . M.elems . counts
