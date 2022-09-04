module DNA (toRNA) where

type RNAChar = Char
type InvalidChar = Char

-- Translate all the characters in the string, and then combine the ``Either``s
-- into a single ``Either``.
toRNA :: String -> Either Char String
toRNA s = mapM translate s

-- Validate and lift a character.
translate :: Char -> Either InvalidChar RNAChar
translate 'C' = Right 'G'
translate 'G' = Right 'C'
translate 'T' = Right 'A'
translate 'A' = Right 'U'
translate x   = Left x
