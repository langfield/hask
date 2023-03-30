import Test.Speculate (speculate, constant, constants, instances, args, reifyInstances, showConstant, Name (..))
import Test.LeanCheck.Instances

import Data.Map (Map)
import qualified Data.Map as M

import Dominoes (chain, Domino)

instance Name (Map a b) where
  name m = "map" 

main :: IO ()
main = speculate args
  { instances = [ reifyInstances (undefined :: [(Int, Int)])
                , reifyInstances (undefined :: Map Int [Int])
                ]
  , constants =
    [ showConstant (1 :: Int)
    , showConstant (0 :: Int)
    ]
  }
