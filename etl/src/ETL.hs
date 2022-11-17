module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Char as C

transform :: Map a String -> Map Char a
transform scoreLetters = M.foldrWithKey updateLetterScores M.empty scoreLetters

updateLetterScores :: a -> String -> Map Char a -> Map Char a
updateLetterScores score letters letterScores = foldr (updateLetterScore score) letterScores letters

updateLetterScore :: a -> Char -> Map Char a -> Map Char a
updateLetterScore score c letterScores = M.insert (C.toLower c) score letterScores
