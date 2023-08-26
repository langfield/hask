{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Connect (Mark(Cross,Nought), winner)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "winner" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion  = winner board `shouldBe` expected

data Case = Case { description :: String
                 , board       :: [String]
                 , expected    :: Maybe Mark
                 }

cases :: [Case]
cases = [
          Case { description = "only edges does not make a winner"
               , board       = [ "O O O X"
                               , " X . . X"
                               , "  X . . X"
                               , "   X O O O" ]
               , expected    = Nothing
               }
        ]
