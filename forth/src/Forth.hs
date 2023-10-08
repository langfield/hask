{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Control.Monad (foldM)
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

-- Function calls just push their implementation onto the stack.
type Namespace = Map Text [Text]
data ForthState = ForthState [Int] Namespace

emptyState :: ForthState
emptyState = ForthState [] M.empty

data Token = Add | Sub | Mul | Div | Dup | Drop | Swap | Over | Word String | Number Int
  deriving Show

parse :: String -> Token
parse "+" = Add
parse "-" = Sub
parse "*" = Mul
parse "/" = Div
parse "dup" = Dup
parse "drop" = Drop
parse "swap" = Swap
parse "over" = Over
parse x
  | all C.isDigit x = Number (read x)
  | otherwise = Word x

go :: ForthState -> Token -> Either ForthError ForthState
go (ForthState xs defs) (Number x) = Right $ ForthState (x : xs) defs
go (ForthState (x : y : rest) defs) Add = Right $ ForthState (x + y : rest) defs
go (ForthState (x : y : rest) defs) Sub = Right $ ForthState (y - x : rest) defs
go (ForthState (x : y : rest) defs) Mul = Right $ ForthState (x * y : rest) defs
go (ForthState (0 : _ : _) _) Div = Left DivisionByZero
go (ForthState (x : y : rest) defs) Div = Right $ ForthState (y `div` x : rest) defs
go (ForthState (x : rest) defs) Dup = Right $ ForthState (x : x : rest) defs
go (ForthState (_ : rest) defs) Drop = Right $ ForthState rest defs
go (ForthState (x : y : rest) defs) Swap = Right $ ForthState (y : x : rest) defs
go (ForthState (x : y : rest) defs) Over = Right $ ForthState (y : x : y : rest) defs
go (ForthState xs defs) (Word w) = Left StackUnderflow
go (ForthState _ _) _ = Left StackUnderflow

rev :: ForthState -> ForthState
rev (ForthState xs m) = ForthState (reverse xs) m

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text stack = fmap rev . foldM go stack . trace' "tokens" . map parse . words . T.unpack $ text

toList :: ForthState -> [Int]
toList (ForthState stack _) = stack
