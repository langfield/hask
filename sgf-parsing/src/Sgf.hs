module Sgf (parseSgf) where

import Control.Applicative (Alternative(..))
import Data.Char (isUpper)
import Data.Functor ((<&>))
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec(..), Parsec, anySingle, anySingleBut, manyTill, parseMaybe, satisfy)
import Text.Megaparsec.Char (char)

-- | The data of a node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type PropMap = Map Text [Text]
type Parser = Parsec Void Text

convert :: [(Char, String)] -> Char -> String
convert m c = fromMaybe [c] (lookup c m)

regularChar :: Parser String
regularChar = anySingleBut '\\' <&> convert [('\t', " ")]

escapeSequence :: Parser String
escapeSequence = char '\\' *> anySingle <&> convert [('\t', " "), ('\n', "")]

charLiteral :: Parser String
charLiteral = regularChar <|> escapeSequence

propName :: Parser Text
propName = T.pack <$> some (satisfy isUpper)

propValue :: Parser Text
propValue = T.pack . concat <$ char '[' <*> manyTill charLiteral (char ']')

prop :: Parser (Text, [Text])
prop = (,) <$> propName <*> some propValue

propMap :: Parser PropMap
propMap = fromList <$ char ';' <*> many prop

node :: Parser (Tree PropMap)
node = do
  m <- propMap
  b <- branch
  pure $ Node m [b]

node' :: Parser (Tree PropMap)
node' = Node <$> propMap <*> many tree

branch :: Parser (Tree PropMap)
branch = try node <|> node'

tree :: Parser (Tree PropMap)
tree = char '(' *> branch <* char ')'

parseSgf :: String -> Maybe (Tree PropMap)
parseSgf = parseMaybe (tree <* eof) . T.pack
