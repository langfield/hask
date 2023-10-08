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

emptyState :: ForthState
emptyState = ForthState [] M.empty

data Token = Colon | Semi | Add | Sub | Mul | Div | Dup | Drop | Swap | Over | Word String | Number Int
  deriving (Eq, Show)

-- Function calls just push their implementation onto the stack.
type Namespace = Map String [Token]
data ForthState = ForthState [Int] Namespace

parse :: String -> Token
parse ":" = Colon
parse ";" = Semi
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

go :: ForthState -> [Token] -> Either ForthError ForthState
go stack [] = Right stack
go (ForthState xs defs) ((Number x) : ts) = go (ForthState (x : xs) defs) ts
go (ForthState (x : y : rest) defs) (Add : ts) = go (ForthState (x + y : rest) defs) ts
go (ForthState (x : y : rest) defs) (Sub : ts) = go (ForthState (y - x : rest) defs) ts
go (ForthState (x : y : rest) defs) (Mul : ts) = go (ForthState (x * y : rest) defs) ts
go (ForthState (0 : _ : _) _) (Div : _) = Left DivisionByZero
go (ForthState (x : y : rest) defs) (Div : ts) = go (ForthState (y `div` x : rest) defs) ts
go (ForthState (x : rest) defs) (Dup : ts) = go (ForthState (x : x : rest) defs) ts
go (ForthState (_ : rest) defs) (Drop : ts) = go (ForthState rest defs) ts
go (ForthState (x : y : rest) defs) (Swap : ts) = go (ForthState (y : x : rest) defs) ts
go (ForthState (x : y : rest) defs) (Over : ts) = go (ForthState (y : x : y : rest) defs) ts
go (ForthState xs defs) (Colon : Word w : ts) = go (ForthState xs defs') ts'
  where
    (def, ts') = break (== Semi) ts
    defs' = M.insert w def defs
go (ForthState xs defs) (Word w : ts) =
  case M.lookup w defs of
    Just def -> go (ForthState xs defs) (def ++ ts)
    Nothing -> Left $ UnknownWord (T.pack w)
go stack (Semi : ts) = go stack ts
go (ForthState _ _) _ = Left StackUnderflow

rev :: ForthState -> ForthState
rev (ForthState xs m) = ForthState (reverse xs) m

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text stack = fmap rev . go stack . trace' "tokens" . map parse . words . T.unpack $ text

toList :: ForthState -> [Int]
toList (ForthState stack _) = stack
