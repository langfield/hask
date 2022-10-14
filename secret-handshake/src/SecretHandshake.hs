module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = getBinaryDigits n


-- Get list of binary digits.
getBinaryDigits :: Int -> [Int]
getBinaryDigits 0 = [0]
getBinaryDigits 1 = [1]
getBinaryDigits n = getBinaryDigits (n `div` 2) ++ getBinaryDigits (n `mod` 2)


getActions :: [Int] -> 
