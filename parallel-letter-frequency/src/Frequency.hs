module Frequency (frequency) where

import Data.Map (Map)
import Data.Text (Text)
import Data.Tuple (swap)

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Control.Parallel.Strategies as P

frequency :: Int -> [Text] -> Map Char Int
frequency _ = foldr (M.unionWith (+) . pmap) M.empty
  where
    pmap :: Text -> Map Char Int
    pmap =
      P.withStrategy P.rpar
        . (M.fromListWith (+)
        . map (curry swap 1)
        . T.unpack
        . T.toLower
        . T.filter C.isAlpha
          )
