module Frequency (frequency) where

import Data.Map (Map)
import Data.Text (Text)
import Data.Tuple (swap)

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T

frequency :: Int -> [Text] -> Map Char Int
frequency _ = foldr
  ( M.unionWith (+)
  . M.fromListWith (+)
  . map (curry swap 1)
  . T.unpack
  . T.toLower
  . T.filter C.isAlpha
  )
  M.empty

frequency2 :: Int -> [Text] -> Map Char Int
frequency2 _ = foldr (M.unionWith (+)) M.empty . pmap
  where
    pmap =
      P.withStrategy rpar
        $ M.fromListWith (+)
        . map (curry swap 1)
        . T.unpack
        . T.toLower
        . T.filter C.isAlpha
