{-# LANGUAGE TemplateHaskell #-}
module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar (Day, toGregorian, fromGregorian)
import Control.Lens.TH (makeLenses)
import Control.Lens.Operators ((^.), (.~), (&))

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

$(makeLenses ''Person)
$(makeLenses ''Name)
$(makeLenses ''Born)
$(makeLenses ''Address)

bornStreet :: Born -> String
bornStreet b = b ^. bornAt . street

setCurrentStreet :: String -> Person -> Person
setCurrentStreet s person = person & (address . street) .~ s

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = person & (born . bornOn) .~ day
  where
    (y, _, d) = toGregorian (person ^. born . bornOn)
    day = fromGregorian y month d

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person & (born . bornAt . street) .~ f s & (address . street) .~ f s'
  where
    s = person ^. born . bornAt . street
    s' = person ^. address . street
