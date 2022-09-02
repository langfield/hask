module DNA (toRNA) where

type RNAChar = Char
type InvalidChar = Char

-- For nonempty strings, validate the first character.
--  * If it's invalid, unwrap and then immediately rewrap with `Left`.
--  * If it's vaiid, recurse and prepend the validated character to the result
--    in the case where the result of the recursion is a ``Right String``.
toRNA :: String -> Either Char String
toRNA s = combine $ map translate s

-- Basically a clone of `Control.Monad.sequence`.
combine :: [Either a b] -> Either a [b]
combine []             = Right []
combine (Left c : _)  = Left c
combine (Right c : cs) = either Left (\xs -> Right (c : xs)) (combine cs)

-- Validate and lift a character.
translate :: Char -> Either InvalidChar RNAChar
translate 'C' = Right 'G'
translate 'G' = Right 'C'
translate 'T' = Right 'A'
translate 'A' = Right 'U'
translate x   = Left x
