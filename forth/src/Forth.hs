{-# LANGUAGE OverloadedStrings #-}

module Forth (ForthError(..), ForthState, evalText, toList, emptyState) where

import Control.Monad ((<=<), (>=>))
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T

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
parse x   = Word x

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
emptyState = ForthState [] $ M.fromList
  [ ("+"   , add')
  , ("-"   , sub')
  , ("*"   , mul')
  , ("/"   , div')
  , ("dup" , dup')
  , ("drop", drop')
  , ("swap", swap')
  , ("over", over')
  ]

getDef :: Namespace -> String -> Either ForthError ([Int] -> Either ForthError [Int])
getDef defs w
  | all C.isDigit w = Right (\xs -> Right (read w : xs))
  | otherwise = maybe unknown Right (M.lookup w defs)
  where unknown = Left . UnknownWord . T.pack $ w

wordName :: Token -> Either ForthError String
wordName (Word name) = Right name
wordName _ = Left InvalidWord

go :: [Int] -> Namespace -> [Token] -> Either ForthError ForthState
go xs defs [] = Right (ForthState xs defs)
go xs defs (Semi : ts) = go xs defs ts
go xs defs (Word w : ts) = getDef defs w >>= run
  where run def = def xs >>= (\xs' -> go xs' defs ts)
go xs defs (Colon : Word w : ts)
  | all C.isDigit w = Left InvalidWord
  | otherwise = define . composeMany =<< mapM (getDef defs <=< wordName) ws
  where
    (ws, ts')   = break (== Semi) ts
    composeMany = foldr (>=>) Right
    define def = go xs (M.insert w def defs) ts'
go _ _ _ = Left StackUnderflow

rev :: ForthState -> ForthState
rev (ForthState xs m) = ForthState (reverse xs) m

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text (ForthState xs defs) =
  fmap rev . go xs defs . map parse . words . map C.toLower . T.unpack $ text

toList :: ForthState -> [Int]
toList (ForthState stack _) = stack
