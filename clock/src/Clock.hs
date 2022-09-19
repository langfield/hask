module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

type Hour = Int
type Minute = Int
data Clock = Clock 
  { cHour :: Hour
  , cMin :: Minute
  } deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock (mod hour 24) (mod minute 60)
  where carry = minute / 60

toString :: Clock -> String
toString clock = printf "%02d" (cHour clock) ++ ":" ++ (printf "%02d" $ cMin clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute clock = Clock (mod (cHour clock + hour) 24) (mod (cMin clock + minute) 60)
