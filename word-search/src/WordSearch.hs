module WordSearch (search, CharPos(..), WordPos(..)) where

import Control.Applicative (asum)
import Data.Map (Map)
import Data.List (sort)
import Data.Maybe (listToMaybe)

import qualified Data.Map as M

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

-- | Map characters in `w` to all their locations in the array.
mklocs :: [String] -> String -> Map Char [(Int,Int)]
mklocs gss w = M.fromListWith (++) [ (c,[(x,y)]) | (y,gs) <- zip [1..] gss, (x,c) <- zip [1..] gs, c `elem` w ]

found :: (Ord k, Eq a) => Map k [a] -> k -> a -> Bool
found locs c coord = coord `elem` M.findWithDefault [] c locs

try :: Ord k => (Int, Int) -> Map k [(Int, Int)] -> [k] -> (Int, Int) -> (Int, Int) -> Maybe WordPos
try (mx,my) locs w (x,y) (dx,dy) =
  listToMaybe [ WordPos (CharPos x y) (CharPos x' y') | x' > 0, x' <= mx, y' > 0, y' <= my, and . zipWith (found locs) w $ rs ]
  where
    k = length w
    (x',y') = (x+dx*(k-1),y+dy*(k-1))
    rs = take k (iterate (\(i,j) -> (i+dx,j+dy)) (x,y))

-- | This initial lookup is pretty inelegant. Can we do better?
searchOne :: [String] -> String -> Maybe WordPos
searchOne _ "" = Nothing
searchOne gss w@(c:_) = M.lookup c locs >>= \ps -> asum [ try (n,m) locs w p d | p <- sort ps, d <- ds ]
   where
     (m,n) = (length gss, maybe 0 length (listToMaybe gss))
     locs = mklocs gss w
     ds = [(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search gss = map (\w -> (w, searchOne gss w))
