{-# LANGUAGE OverloadedStrings #-}
module Forth (ForthError (..), ForthState, evalText, toList, emptyState) where
import Data.Char (isNumber, toLower)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

data ForthError
  = DivisionByZero
  | StackUnderflow
  | InvalidWord
  | UnknownWord Text
  deriving (Show, Eq)

data Token = Number Int | Symbol Text deriving (Show, Eq)
type ForthState = (Map Text [Token], [Int])

emptyState :: ForthState
emptyState = (M.empty, [])

parseToken :: Text -> Token
parseToken token
  | T.all isNumber token = Number . read . T.unpack $ token
  | otherwise = Symbol . T.map toLower $ token

-- | Inline word definitions (but only 1 level deep).
inline :: Map Text [Token] -> [Token] -> [Token]
inline _ [] = []
inline vars (Number n : expr) = Number n : inline vars expr
inline vars (Symbol s : expr) = case M.lookup s vars of
  Just def -> def ++ inline vars expr
  Nothing -> Symbol s : inline vars expr

isBuiltin :: Text -> Bool
isBuiltin sym = sym `elem` ["dup", "drop", "swap", "over", "+", "-", "/", "*"]

-- | Recursively evaluate a token stream given some state.
eval :: [Token] -> ForthState -> Either ForthError ForthState
eval [] result = Right result
eval (Number n : ts) (vars, xs) = eval ts (vars, n : xs)
eval (Symbol s : ts) (vars, xs)
  | Just def <- M.lookup s vars = eval (def ++ ts) (vars, xs)
eval (Symbol ":" : Number _ : _) (_, _) = Left InvalidWord
eval (Symbol ":" : ts) (vars, xs) = eval rest (vars', xs)
  where
    vars' = M.insert s (inline vars def) vars
    (Symbol s : def, Symbol ";" : rest) = span (/= Symbol ";") ts
eval (Symbol "dup" : ts) (vars, x : xs) = eval ts (vars, x : x : xs)
eval (Symbol "drop" : ts) (vars, _ : xs) = eval ts (vars, xs)
eval (Symbol "swap" : ts) (vars, x : y : rest) = eval ts (vars, y : x : rest)
eval (Symbol "over" : ts) (vars, x : y : rest) = eval ts (vars, y : x : y : rest)
eval (Symbol "+" : ts) (vars, x : y : rest) = eval ts (vars, (y + x) : rest)
eval (Symbol "-" : ts) (vars, x : y : rest) = eval ts (vars, (y - x) : rest)
eval (Symbol "*" : ts) (vars, x : y : rest) = eval ts (vars, (y * x) : rest)
eval (Symbol "/" : _) (_, 0 : _ : _) = Left DivisionByZero
eval (Symbol "/" : ts) (vars, x : y : rest) = eval ts (vars, (y `div` x) : rest)
eval (Symbol s : _) _
  | not (isBuiltin s) = Left (UnknownWord s)
  | otherwise = Left StackUnderflow

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text = eval $ parseToken <$> T.words text

toList :: ForthState -> [Int]
toList = reverse . snd
