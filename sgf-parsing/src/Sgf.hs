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

type Parser = Parsec Void Text

-- The semicolon kinda means, "Everything after this is a single child node of
-- the previous node."

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf = MP.parseMaybe parseNothing

parseNothing :: Parser (Tree (Map Text [Text]))
parseNothing = do
  _ <- MPC.char '('
  _ <- MPC.char ';'
  _ <- MPC.char ')'
  pure (Node M.empty [])
