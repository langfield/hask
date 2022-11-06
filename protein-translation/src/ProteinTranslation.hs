module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins ('A' : 'U' : 'G' : cs) = ("Methionine" :) <$> proteins cs
proteins ('U' : 'U' : 'U' : cs) = ("Phenylalanine" :) <$> proteins cs
proteins ('U' : 'U' : 'C' : cs) = ("Phenylalanine" :) <$> proteins cs
proteins ('U' : 'U' : 'A' : cs) = ("Leucine" :) <$> proteins cs
proteins ('U' : 'U' : 'G' : cs) = ("Leucine" :) <$> proteins cs
proteins ('U' : 'C' : 'U' : cs) = ("Serine" :) <$> proteins cs
proteins ('U' : 'C' : 'C' : cs) = ("Serine" :) <$> proteins cs
proteins ('U' : 'C' : 'A' : cs) = ("Serine" :) <$> proteins cs
proteins ('U' : 'C' : 'G' : cs) = ("Serine" :) <$> proteins cs
proteins ('U' : 'A' : 'U' : cs) = ("Tyrosine" :) <$> proteins cs
proteins ('U' : 'A' : 'C' : cs) = ("Tyrosine" :) <$> proteins cs
proteins ('U' : 'G' : 'U' : cs) = ("Cysteine" :) <$> proteins cs
proteins ('U' : 'G' : 'C' : cs) = ("Cysteine" :) <$> proteins cs
proteins ('U' : 'G' : 'G' : cs) = ("Tryptophan" :) <$> proteins cs
proteins ('U' : 'A' : 'A' : _) = Just []
proteins ('U' : 'A' : 'G' : _) = Just []
proteins ('U' : 'G' : 'A' : _) = Just []
proteins [] = Just []
proteins _ = Nothing
