module FoodChain (song) where

{-
"I know an old lady who swallowed a fly.\n\
\I don't know why she swallowed the fly. Perhaps she'll die.\n\
\\n\
\I know an old lady who swallowed a spider.\n\
\It wriggled and jiggled and tickled inside her.\n\
\She swallowed the spider to catch the fly.\n\
\I don't know why she swallowed the fly. Perhaps she'll die.\n\
\\n\
\I know an old lady who swallowed a bird.\n\
\How absurd to swallow a bird!\n\
\She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
\She swallowed the spider to catch the fly.\n\
\I don't know why she swallowed the fly. Perhaps she'll die.\n\
\\n\
\I know an old lady who swallowed a cat.\n\
\Imagine that, to swallow a cat!\n\
\She swallowed the cat to catch the bird.\n\
\She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
\She swallowed the spider to catch the fly.\n\
\I don't know why she swallowed the fly. Perhaps she'll die.\n\
\\n\
\I know an old lady who swallowed a dog.\n\
\What a hog, to swallow a dog!\n\
\She swallowed the dog to catch the cat.\n\
\She swallowed the cat to catch the bird.\n\
\She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
\She swallowed the spider to catch the fly.\n\
\I don't know why she swallowed the fly. Perhaps she'll die.\n\
\\n\
\I know an old lady who swallowed a goat.\n\
\Just opened her throat and swallowed a goat!\n\
\She swallowed the goat to catch the dog.\n\
\She swallowed the dog to catch the cat.\n\
\She swallowed the cat to catch the bird.\n\
\She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
\She swallowed the spider to catch the fly.\n\
\I don't know why she swallowed the fly. Perhaps she'll die.\n\
\\n\
\I know an old lady who swallowed a cow.\n\
\I don't know how she swallowed a cow!\n\
\She swallowed the cow to catch the goat.\n\
\She swallowed the goat to catch the dog.\n\
\She swallowed the dog to catch the cat.\n\
\She swallowed the cat to catch the bird.\n\
\She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
\She swallowed the spider to catch the fly.\n\
\I don't know why she swallowed the fly. Perhaps she'll die.\n\
\\n\
\I know an old lady who swallowed a horse.\n\
\She's dead, of course!\n"
-}

type Animal = String
type Line = String

song :: String
song = concat $ concat
      [ [know "fly", exclamation "fly", "\n"]
      , [know "spider", exclamation "spider", reason "spider" "fly", exclamation "fly", "\n"]
      , [know "bird", exclamation "bird", reason "bird" "spider that wriggled and jiggled and tickled inside her", reason "spider" "fly", exclamation "fly", "\n"]
      , [know "cat", exclamation "cat", reason "cat" "bird", reason "bird" "spider that wriggled and jiggled and tickled inside her", reason "spider" "fly", exclamation "fly", "\n"]
      , [know "dog", exclamation "dog", reason "dog" "cat", reason "cat" "bird", reason "bird" "spider that wriggled and jiggled and tickled inside her", reason "spider" "fly", exclamation "fly", "\n"]
      , [know "goat", exclamation "goat", reason "goat" "dog", reason "dog" "cat", reason "cat" "bird", reason "bird" "spider that wriggled and jiggled and tickled inside her", reason "spider" "fly", exclamation "fly", "\n"]
      , [know "cow", exclamation "cow", reason "cow" "goat", reason "goat" "dog", reason "dog" "cat", reason "cat" "bird", reason "bird" "spider that wriggled and jiggled and tickled inside her", reason "spider" "fly", exclamation "fly", "\n"]
      , [know "horse", exclamation "horse"]
      ]
  where animals = ["fly", "spider", "bird", "cat", "dog", "goat", "cow"]

know :: Animal -> Line
know a = "I know an old lady who swallowed a " ++ a ++ ".\n"

reason :: Animal -> Animal -> Line
reason a a' = "She swallowed the " ++ a ++ " to catch the " ++ a' ++ ".\n"

exclamation :: Animal -> Line
exclamation "fly" = "I don't know why she swallowed the fly. Perhaps she'll die.\n"
exclamation "spider" = "It wriggled and jiggled and tickled inside her.\n"
exclamation "bird" = "How absurd to swallow a bird!\n"
exclamation "cat" = "Imagine that, to swallow a cat!\n"
exclamation "dog" = "What a hog, to swallow a dog!\n"
exclamation "goat" = "Just opened her throat and swallowed a goat!\n"
exclamation "cow" = "I don't know how she swallowed a cow!\n"
exclamation "horse" = "She's dead, of course!\n"
exclamation b = "Goodness me, to swallow a " ++ b ++ "!\n"
