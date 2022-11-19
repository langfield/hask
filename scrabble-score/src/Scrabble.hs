module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as M

letterListAssocs :: [([Char], Integer)]
letterListAssocs = [
  (['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'],      1),
  (['D', 'G'],                                              2),
  (['B', 'C', 'M', 'P'],                                    3),
  (['F', 'H', 'V', 'W', 'Y'],                               4),
  (['K'],                                                   5),
  (['J', 'X'],                                              8),
  (['Q', 'Z'],                                              10)]

letterScores :: Map Char Integer
letterScores = (M.fromList . concatMap (uncurry unfoldListAssoc)) letterListAssocs

unfoldListAssoc :: [Char] -> Integer -> [(Char, Integer)]
unfoldListAssoc [] _ = []
unfoldListAssoc (x : xs) n = (x, n) : unfoldListAssoc xs n

scoreLetter :: Char -> Integer
scoreLetter letter = M.findWithDefault 0 (toUpper letter) letterScores

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
