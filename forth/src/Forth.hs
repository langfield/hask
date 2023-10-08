{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Control.Monad ((>=>))
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T

import Debug.Trace (trace)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data Token = Colon | Semi | Word String
  deriving (Eq, Show)

-- Function calls just push their implementation onto the stack.
type Namespace = Map String ([Int] -> Either ForthError [Int])
data ForthState = ForthState [Int] Namespace

parse :: String -> Token
parse ":" = Colon
parse ";" = Semi
parse x = Word x

add' :: [Int] -> Either ForthError [Int]
add' (x : y : rest) = Right (x + y : rest)
add' _ = Left StackUnderflow

sub' :: [Int] -> Either ForthError [Int]
sub' (x : y : rest) = Right (y - x : rest)
sub' _ = Left StackUnderflow

mul' :: [Int] -> Either ForthError [Int]
mul' (x : y : rest) = Right (x * y : rest)
mul' _ = Left StackUnderflow

div' :: [Int] -> Either ForthError [Int]
div' (0 : _ : _) = Left DivisionByZero
div' (x : y : rest) = Right (y `div` x : rest)
div' _ = Left StackUnderflow

dup' :: [Int] -> Either ForthError [Int]
dup' (x : rest) = Right (x : x : rest)
dup' _ = Left StackUnderflow

drop' :: [Int] -> Either ForthError [Int]
drop' (_ : rest) = Right rest
drop' _ = Left StackUnderflow

swap' :: [Int] -> Either ForthError [Int]
swap' (x : y : rest) = Right (y : x : rest)
swap' _ = Left StackUnderflow

over' :: [Int] -> Either ForthError [Int]
over' (x : y : rest) = Right (y : x : y : rest)
over' _ = Left StackUnderflow

emptyState :: ForthState
emptyState = ForthState [] $ M.fromList [("+", add'), ("-", sub'), ("*", mul'), ("/", div'), ("dup", dup'), ("drop", drop'), ("swap", swap'), ("over", over')]

getDef :: Namespace -> Token -> Either ForthError ([Int] -> Either ForthError [Int])
getDef defs (Word name)
  | all C.isDigit name = Right (\xs -> Right (read name : xs))
  | otherwise =
    case M.lookup (map C.toLower name) defs of
      Just def -> Right def
      Nothing -> Left $ UnknownWord (T.pack name)
getDef _ _ = Left InvalidWord

go :: ForthState -> [Token] -> Either ForthError ForthState
go stack [] = Right stack
go (ForthState xs defs) (Colon : Word w : ts)
  | all C.isDigit w = Left InvalidWord
  | otherwise =
    case foldr (>=>) Right <$> mapM (getDef defs) ws of
      Right def -> go (ForthState xs (M.insert (map C.toLower w) def defs)) ts'
      Left err -> Left err
    where
      (ws, ts') = break (== Semi) ts
go (ForthState xs defs) (Word w : ts) =
  case getDef defs (Word w) of
    Right def ->
      case def xs of
        Right xs' -> go (ForthState xs' defs) ts
        Left err -> Left err
    Left err -> Left err
go stack (Semi : ts) = go stack ts
go (ForthState _ _) _ = Left StackUnderflow

rev :: ForthState -> ForthState
rev (ForthState xs m) = ForthState (reverse xs) m

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text stack = fmap rev . go stack . trace' "tokens" . map parse . words . T.unpack $ text

toList :: ForthState -> [Int]
toList (ForthState stack _) = stack
