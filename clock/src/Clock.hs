module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

newtype Clock = Clock Int deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock $ (hour * 60 + minute) `mod` 1440

toString :: Clock -> String
toString (Clock m) = printf "%02d:%02d" hour minute
  where hour = m `div` 60
        minute = m `mod` 60

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock m) = fromHourMin hour $ minute + m
