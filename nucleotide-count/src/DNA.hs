{-# LANGUAGE TupleSections #-}

module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromListWith)

type Error = String

-- I have always struggled with how inductive type definitions work
-- behind-the-scenes. I have stared at a lot of Agda, Idris 2, and Lean 4 code
-- over the past year, and when you're defining a sum type, I always get
-- confused about how the constructors can be well-defined when all we give are
-- the type signatures. Don't we have to provide actual 'implementations' of
-- any functions we write?
-- 
-- Not so applicable here, but it still throws me a bit to see these 'variable
-- names' ``A``, etc. just floating here, having not been previously defined
-- anywhere else.
data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either Error (Map Nucleotide Int)
nucleotideCounts xs = fromListWith (+) . map (, 1) <$> mapM translate xs

-- Validate and lift a character.
translate :: Char -> Either Error Nucleotide
translate 'C' = Right C
translate 'G' = Right G
translate 'T' = Right T
translate 'A' = Right A
translate _   = Left "error"
