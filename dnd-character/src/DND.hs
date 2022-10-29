module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Data.List (sort, sortBy)
import Test.QuickCheck (Gen, choose, vectorOf)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)


modifier :: Int -> Int
modifier n = floor $ (fromIntegral $ n - 10 :: Double) / 2

ability :: Gen Int
ability = sum <$> take 3 <$> sort <$> vectorOf 6 (choose (1, 6))

character :: Gen Character
character = do
  strength <- ability
  dexterity <- ability
  constitution <- ability
  intelligence <- ability
  wisdom <- ability
  charisma <- ability
  let hitpoints = 10 + (modifier constitution)
  return (Character strength dexterity constitution intelligence wisdom charisma hitpoints)
