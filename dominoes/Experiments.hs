import Test.Speculate (speculate, constant, constants, instances, args, reifyInstances, showConstant, Name (..))
import Test.LeanCheck.Instances

import Data.Map (Map)
import qualified Data.Map as M

import Dominoes (chain, insert, remove, search, length', State, Domino)

instance Name (Map a b) where
  name m = "map" 

main :: IO ()
main = speculate args
  { instances = [ reifyInstances (undefined :: [(Int, Int)])
                , reifyInstances (undefined :: Map Int [Int])
                , reifyInstances (undefined :: State)
                ]
  , constants =
    [ showConstant (([], M.empty) :: State)
    , showConstant (1 :: Int)
    , showConstant (0 :: Int)
    , constant "insert" (insert :: Domino -> State -> State)
    , constant "remove" (remove :: Domino -> State -> State)
    , constant "length'" (length' :: State -> Int)
    ]
  }
