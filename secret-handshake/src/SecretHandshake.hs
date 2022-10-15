module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = if sixteens == 1 then reverse ops else ops
  where ones = n `mod` 2
        twos = (n `div` 2) `mod` 2
        fours = (n `div` 4) `mod` 2
        eights = (n `div` 8) `mod` 2
        sixteens = (n `div` 16) `mod` 2
        ops = [] 
              ++ (if ones == 1 then ["wink"] else [])
              ++ (if twos == 1 then ["double blink"] else [])
              ++ (if fours == 1 then ["close your eyes"] else [])
              ++ (if eights == 1 then ["jump"] else [])
