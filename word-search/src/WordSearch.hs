module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Bifunctor (bimap)
import Data.List (findIndex, isPrefixOf, tails, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Tuple (swap)

import Debug.Trace (trace)

data Dir = U | D | L | R | TLBR | TRBL | BLTR | BRTL deriving (Eq, Show)

tt s x = trace (s ++ ": " ++ show x) x

mkpos :: (Int,Int) -> CharPos
mkpos (x,y) = CharPos (y+1) (x+1)

data CharPos = CharPos {col :: Int, row :: Int} deriving (Eq, Show)
data WordPos = WordPos {start :: CharPos, end :: CharPos} deriving (Eq, Show)

trans :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
trans (a,b) = (swap a, swap b)

reflect :: Int -> ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
reflect m ((a,b),(c,d)) = ((m-a,b),(m-c,d))

shift :: Int -> (Int,Int) -> (Int,Int)
shift n (x,y) | n > 0 = shift (n-1) (x+1,y+1)
shift _ a = a

undiag :: Int -> ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
undiag n (u,v) = (go u, go v)
  where
    go (x,y)
      | x <= n = shift y (0,n-x)
      | otherwise = shift y (x-n,0)

mkloc :: String -> Int -> Int -> ((Int,Int),(Int,Int))
mkloc w i j = ((i,j),(i,j + length w - 1))

searchRow :: String -> Int -> String -> Maybe ((Int,Int),(Int,Int))
searchRow w i = fmap (mkloc w i) . (findIndex (w `isPrefixOf`) . tails)

searchWord :: String -> [String] -> [((Int,Int),(Int,Int))]
searchWord w = catMaybes . zipWith (searchRow w) [0..]

bisearch :: String -> [String] -> [((Int,Int),(Int,Int))]
bisearch w xss = searchWord w xss ++ map swap (searchWord (reverse w) xss)

quadsearch :: String -> [String] -> [((Int,Int),(Int,Int))]
quadsearch w xss = bisearch w xss ++ map trans (bisearch w (transpose xss))

rot :: [String] -> [String]
rot = transpose . reverse

diags :: [String] -> [String]
diags xss = reverse top ++ bot
  where
    top = transpose $ zipWith drop [0..] xss
    bot = transpose $ zipWith drop [1..] $ transpose xss

diagsearch :: String -> [String] -> [((Int,Int),(Int,Int))]
diagsearch w xss = map (trans . undiag n) (bisearch w (diags xss))
-- ++ map (reflect m . undiag n) (bisearch w (diags (rot xss)))
  where
    m = length xss
    n = maybe 0 length (listToMaybe xss)

octosearch :: String -> [String] -> [((Int,Int),(Int,Int))]
octosearch w xss = quadsearch w xss ++ diagsearch w xss

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid = map (\w -> (w, fmap (uncurry WordPos . bimap mkpos mkpos) <$> listToMaybe $ octosearch w grid))
