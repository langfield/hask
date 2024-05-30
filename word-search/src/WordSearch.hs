module WordSearch (search, CharPos(..), WordPos(..)) where

import Control.Applicative (asum)
import Data.Map (Map)
import Data.List (sort)
import Data.Maybe (listToMaybe)

import qualified Data.Map as M

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search gss = map (\w -> (w, searchOne gss w))

mkMap :: [String] -> String -> M.Map Char [(Int,Int)]
mkMap gss w = M.fromListWith (++) [ (c,[(x,y)]) | (y,gs) <- zip [1..] gss, (x,c) <- zip [1..] gs, c `elem` w ]

searchOne :: [String] -> String -> Maybe WordPos
searchOne _ "" = Nothing
searchOne gss w@(c:_) = M.lookup c locs >>= \ps -> asum [ try mx my k locs w p d | p <- sort ps, d <- ds ]
   where
     locs = mkMap gss w
     (k, mx, my) = (length w, maybe 0 length (listToMaybe gss), length gss)
     ds = [(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]

try :: Ord k => Int -> Int -> Int -> Map k [(Int, Int)] -> [k] -> (Int, Int) -> (Int, Int) -> Maybe WordPos
try mx my k locs w (x,y) (dx,dy) =
  listToMaybe [ WordPos (CharPos x y) (CharPos x' y') | x' > 0, x' <= mx, y' > 0, y' <= my, test locs w rs ]
  where
    (x',y') = (x+dx*(k-1),y+dy*(k-1))
    rs = take k (iterate (\(i,j) -> (i+dx,j+dy)) (x,y))

test :: (Eq a, Ord k) => Map k [a] -> [k] -> [a] -> Bool
test locs w = and . zipWith (\c p -> p `elem` M.findWithDefault [] c locs) w
