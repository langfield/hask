module PigLatin (translate) where

translate :: String -> String
translate = unwords . map go . words
  where
    go "" = ""
    go s@('x':'r':_) = s ++ "ay"
    go s@('y':'t':_) = s ++ "ay"
    go ('c':'h':cs) = cs ++ "chay"
    go ('s':'c':'h':cs) = cs ++ "schay"
    go ('t':'h':'r':cs) = cs ++ "thray"
    go ('t':'h':cs) = cs ++ "thay"
    go ('r':'h':'y':cs) = 'y':cs ++ "rhay"
    go s@(c:cs)
      | c `elem` "aeiou" = s ++ "ay"
      | otherwise =
        case s of
          'q':'u':ds -> ds ++ "quay"
          d:'q':'u':ds -> ds ++ [d] ++ "quay"
          _ -> cs ++ [c] ++ "ay"
