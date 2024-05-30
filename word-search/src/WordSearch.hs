module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.List (findIndex, isPrefixOf, tails, transpose)
import Data.Maybe (catMaybes, listToMaybe)

import Debug.Trace (trace)
tt s x = trace (s ++ ": " ++ show x) x

data CharPos = CharPos {col :: Int, row :: Int} deriving (Eq, Show)
data WordPos = WordPos {start :: CharPos, end :: CharPos} deriving (Eq, Show)

rev :: WordPos -> WordPos
rev (WordPos a b) = WordPos b a

trans :: WordPos -> WordPos
trans (WordPos (CharPos a b) (CharPos c d)) = WordPos (CharPos b a) (CharPos d c)

reflect :: [String] -> WordPos -> WordPos
reflect xss (WordPos (CharPos a b) (CharPos c d)) = WordPos (CharPos a (m-b+1)) (CharPos c (m-d+1))
  where
    m = length xss

diagShift :: Int -> CharPos -> CharPos
diagShift 0 c = c
diagShift n (CharPos a b)
  | n < 0 = CharPos a b
  | otherwise = diagShift (n-1) (CharPos (a+1) (b+1))

undiag :: [String] -> CharPos -> CharPos
undiag xss (CharPos a b)
  | b <= n = diagShift (a-1) $ CharPos 1 (n-b+1)
  | otherwise = diagShift (a-1) $ CharPos (b-n+1) 1
  where
    n = maybe 0 length (listToMaybe xss)

undiag' :: [String] -> WordPos -> WordPos
undiag' xss (WordPos u v) = WordPos (undiag xss u) (undiag xss v)

mkpos :: String -> Int -> Int -> WordPos
mkpos w i j = WordPos (CharPos (j+1) i) (CharPos (j + length w) i)

searchRow :: String -> Int -> String -> Maybe WordPos
searchRow w i = fmap (mkpos w i) . (findIndex (w `isPrefixOf`) . tails)

searchWord :: [String] -> String -> [WordPos]
searchWord xss w = catMaybes $ zipWith (searchRow w) [1..] xss

bisearch :: [String] -> String -> [WordPos]
bisearch xss w = searchWord xss w ++ map rev (searchWord xss (reverse w))

quadsearch :: [String] -> String -> [WordPos]
quadsearch xss w = bisearch xss w ++ map trans (bisearch (transpose xss) w)

rot :: [String] -> [String]
rot = transpose . reverse

diags :: [String] -> [String]
diags xss = reverse top ++ bot
  where
    top = transpose $ zipWith drop [0..] xss
    bot = transpose $ zipWith drop [1..] $ transpose xss

diagsearch :: [String] -> String -> [WordPos]
diagsearch xss w = map (trans . undiag' xss) (bisearch (diags xss) w) ++ map (reflect xss . undiag' xss) (bisearch (diags (rot xss)) w)

octosearch :: [String] -> String -> [WordPos]
octosearch xss w = quadsearch xss w ++ diagsearch xss w

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid = map (\w -> (w, listToMaybe $ octosearch grid w))
