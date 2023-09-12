{-# LANGUAGE OverloadedStrings #-}
module Sgf (parseSgf) where

import Data.Map  (Map)
import Data.Void (Void)
import Data.Text (Text)
import Data.Tree (Tree (..))
import Text.Megaparsec (Parsec)

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Control.Applicative.Combinators as AC

type Parser = Parsec Void Text

-- The semicolon kinda means, "Everything after this is a single child node of
-- the previous node."

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf = MP.parseMaybe parseNodes

parseProperty :: Parser (Text, [Text])
parseProperty = do
  name <- AC.some MPC.upperChar
  contents <- AC.some parsePropertyValue
  pure (T.pack name, contents)

parsePropertyValue :: Parser Text
parsePropertyValue = do
  _ <- MPC.char '['
  p <- AC.some MPC.letterChar
  _ <- MPC.char ']'
  pure (T.pack p)

parseNode :: Parser (Tree (Map Text [Text]))
parseNode = do
  _ <- MPC.char ';'
  ps <- AC.many parseProperty
  pure (Node (M.fromList ps) [])

parseNodes :: Parser (Tree (Map Text [Text]))
parseNodes = do
  _ <- MPC.char '('
  nodes <- AC.some parseNode
  _ <- MPC.char ')'
  case nodes of
    [] -> pure (Node M.empty [])
    (Node m children : ns) -> pure (Node m (children ++ ns))
