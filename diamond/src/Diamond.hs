module Diamond (diamond) where

stagger :: [[Char]] -> [[Char]]
stagger = zipWith (++) [replicate i ' ' | i <- [0..]]

mirror :: [a] -> [a]
mirror [] = []
mirror (x:xs) = reverse xs ++ (x:xs)

diamond :: Char -> Maybe [String]
diamond c
  | c `notElem` ['A'..'Z'] = Nothing
  | otherwise = Just
              . mirror
              . reverse
              . map mirror
              . reverse
              . map reverse
              . stagger
              . map reverse
              . reverse
              . stagger
              $ [[d] | d <- ['A'..c]]
