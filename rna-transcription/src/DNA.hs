module DNA (toRNA) where

type RNAChar = Char
type InvalidChar = Char

-- Translate all the characters in the string, and then combine the ``Either``s
-- into a single ``Either``.
toRNA :: String -> Either Char String
toRNA s = combine $ map translate s

-- Basically a clone of ``Control.Monad.sequence``.
combine :: [Either a b] -> Either a [b]
combine []             = Right []
combine (Left c : _)   = Left c
combine (Right c : cs) = either Left (\xs -> Right (c : xs)) (combine cs)

-- Validate and lift a character.
translate :: Char -> Either InvalidChar RNAChar
translate 'C' = Right 'G'
translate 'G' = Right 'C'
translate 'T' = Right 'A'
translate 'A' = Right 'U'
translate x   = Left x
