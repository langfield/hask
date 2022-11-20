module Isogram (isIsogram) where

import Data.Char (toLower)
import qualified Data.Set as S

isIsogram :: String -> Bool
isIsogram s
  | length (S.fromList t) == length t = True
  | otherwise = False
  where
    t = map toLower $ filter (\c -> notElem c "- ") s
