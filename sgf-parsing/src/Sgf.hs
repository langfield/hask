{-# LANGUAGE OverloadedStrings #-}
module Sgf (parseSgf) where

import Control.Applicative ((<|>))
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
type PropertyMap = Map Text [Text]
data Descendants = Nested [Tree PropertyMap] | Flat [Tree PropertyMap] deriving Show

-- The semicolon kinda means, "Everything after this is a single child node of
-- the previous node."

parseSgf :: Text -> Maybe (Tree PropertyMap)
parseSgf = MP.parseMaybe parseNodes

parseProperty :: Parser (Text, [Text])
parseProperty = do
  name <- AC.some MPC.upperChar
  contents <- AC.some parsePropertyValue
  pure (T.pack name, contents)

parseEscaped :: Parser String
parseEscaped = do
  _ <- MPC.char '\\'
  c <- MP.satisfy (`elem` ['\\', ']', '\t', '\n', 't', 'n'])
  pure $ case c of
    '\n' -> "\\\n"
    c' -> [c']

parseUnescaped :: Parser String
parseUnescaped = do
  c <- MP.satisfy (`notElem` ['\\', ']'])
  pure [c]

parsePropertyValuePith :: Parser Text
parsePropertyValuePith = do
  s <- AC.some (parseEscaped <|> parseUnescaped)
  pure . T.pack . concat $ s

parsePropertyValue :: Parser Text
parsePropertyValue = do
  _ <- MPC.char '['
  s <- parsePropertyValuePith
  _ <- MPC.char ']'
  pure (T.replace "\t" " " . T.replace "\\\\" "\\" . T.replace "\\\n" "" $ s)

parseNode :: Parser PropertyMap
parseNode = do
  _ <- MPC.char ';'
  ps <- AC.many parseProperty
  pure (M.fromList ps)

stackMaps :: [PropertyMap] -> Tree PropertyMap
stackMaps [] = Node M.empty []
stackMaps [m] = Node m []
stackMaps (m : ms) = Node m [stackMaps ms]

parseNodes :: Parser (Tree PropertyMap)
parseNodes = do
  _ <- MPC.char '('
  maps <- AC.some parseNode
  children <- AC.many parseNodes
  _ <- MPC.char ')'
  case maps of
    [] -> pure (Node M.empty [])
    [m] -> pure (Node m children)
    ms -> pure (stackMaps ms)
