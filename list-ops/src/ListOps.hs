{-# LANGUAGE BangPatterns #-}
module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f !z (x:xs) = foldl' f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

(++) :: [a] -> [a] -> [a]
[] ++ [] = []
xs ++ [] = xs
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss
