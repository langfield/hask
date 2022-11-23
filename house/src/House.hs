module House (rhyme) where

import Data.List (intercalate, tails)

assocs :: [(String, String)]
assocs = [ ("horse and the hound and the horn", "")
         , ("farmer sowing his corn", "belonged to")
         , ("rooster that crowed in the morn", "kept")
         , ("priest all shaven and shorn", "woke")
         , ("man all tattered and torn", "married")
         , ("maiden all forlorn", "kissed")
         , ("cow with the crumpled horn", "milked")
         , ("dog", "tossed")
         , ("cat", "worried")
         , ("rat", "killed")
         , ("malt", "ate")
         , ("house that Jack built", "lay in")
         ]


-- Get all possible nonempty tails of the associations list, compute a stanza
-- from each one, then reverse then order of the stanzas, and then join them
-- all with newlines.
rhyme :: String
rhyme = intercalate "\n" $ reverse $ map stanza $ init $ tails assocs

-- Print the first line, then intercalate all the other lines with newlines,
-- and print periods accordingly.
stanza :: [(String, String)] -> String
stanza [] = ""
stanza ((entity, _) : xs) =
  case thatLines xs of
    [] -> firstLine ++ ".\n"
    ys -> firstLine ++ "\n" ++ intercalate "\n" ys ++ ".\n"
  where
    firstLine = "This is the " ++ entity

-- Compute lines beginning with `that` from a list of associations.
thatLines :: [(String, String)] -> [String]
thatLines [] = []
thatLines xs = map format xs

-- Format an association into a line.
format :: (String, String) -> String
format (entity, conjugation) = "that " ++ conjugation ++ " the " ++ entity
