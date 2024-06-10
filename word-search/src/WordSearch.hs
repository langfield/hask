module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Map (Map)
import Data.Set (Set, member)
import Data.List (sort)
import Data.Maybe (listToMaybe)

import qualified Data.Map as M
import qualified Data.Set as S

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

-- | O(max(mn,w*log(w)))
-- O(w*log(w)) for the `find` and O(mn) for the `elem`
found :: (Ord k, Eq a) => Map k [a] -> k -> a -> Bool
found locs c coord = coord `elem` M.findWithDefault [] c locs

-- | O(w*max(mn,w*log(w)))
try :: Ord k => (Int, Int) -> Map k [(Int, Int)] -> [k] -> (Int, Int) -> (Int, Int) -> [WordPos]
try (n,m) locs w (x,y) (dx,dy) =
  [ WordPos (CharPos x y) (CharPos x' y') | x' > 0, x' <= n, y' > 0, y' <= m, and . zipWith (found locs) w $ rs ]
  where
    k = length w
    (x',y') = (x+dx*(k-1),y+dy*(k-1))
    rs = take k (iterate (\(i,j) -> (i+dx,j+dy)) (x,y))

-- | This initial lookup is pretty inelegant. Can we do better?
--
-- O(mn*log(mn))
searchOne :: [String] -> String -> Maybe WordPos
searchOne _ "" = Nothing
searchOne gss w@(c:_) = M.lookup c locs >>= \ps -> listToMaybe $ concat [ try (n,m) locs w p d | p <- sort ps, d <- ds ]
   where
     (m,n) = (length gss, maybe 0 length (listToMaybe gss))
     locs = mklocs gss w
     ds = [(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]

-- =============================================================================

mklocs :: [String] -> Set Char -> Map Char [(Int,Int)]
mklocs zss w = M.fromListWith (++) [ (c,[(j,i)]) | (i,zs) <- zip [1..] zss, (j,c) <- zip [1..] zs, c `member` w ]

positions :: [String] -> [String] -> Map String WordPos
positions zss = undefined . S.fromList . concat

lookupWords :: [String] -> Map String WordPos -> [(String, Maybe WordPos)]
lookupWords ws m = [(w, M.lookup w m) | w <- ws]

-- O(w)
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search zss ws = lookupWords ws (positions zss ws)

-- Search for every word.
-- Find every starting character of every word.
-- For each starting character, search for all words with that starting character in each direction.
--
-- Map starting characters to lists of words.
-- Map characters in all words to their locations in the array.
