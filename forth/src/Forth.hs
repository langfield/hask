{-# LANGUAGE OverloadedStrings #-}
module Forth (ForthError (..), ForthState, evalText, toList, emptyState) where
import Data.Char (isNumber, toLower)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
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

resolve :: Map Text [Token] -> [Token] -> [Token]
resolve _ [] = []
resolve vars (Number n : expr) = Number n : resolve vars expr
resolve vars (Symbol s : expr) = case M.lookup s vars of
  Just def -> def ++ resolve vars expr
  Nothing -> Symbol s : resolve vars expr

isBuiltin :: Text -> Bool
isBuiltin sym = sym `elem` ["dup", "drop", "swap", "over", "+", "-", "/", "*"]

-- | Recursively evaluate a token stream given some state.
eval :: [Token] -> ForthState -> Either ForthError ForthState
eval [] result = Right result
eval (Number n : os) (vars, xs) = eval os (vars, n : xs)
eval (Symbol s : os) (vars, xs)
  | isJust def = eval (fromJust def ++ os) (vars, xs)
  where
    def = M.lookup s vars
eval (Symbol ":" : Number _ : _) (_, _) = Left InvalidWord
eval (Symbol ":" : os) (vars, xs) = eval rest (newVars, xs)
  where
    newVars = M.insert s (resolve vars definition) vars
    (Symbol s : definition, Symbol ";" : rest) = span (/= Symbol ";") os
eval (Symbol "dup" : os) (vars, x : xs) = eval os (vars, x : x : xs)
eval (Symbol "drop" : os) (vars, _ : xs) = eval os (vars, xs)
eval (Symbol "swap" : os) (vars, x : y : rest) = eval os (vars, y : x : rest)
eval (Symbol "over" : os) (vars, x : y : rest) = eval os (vars, y : x : y : rest)
eval (Symbol "+" : os) (vars, x : y : rest) = eval os (vars, (y + x) : rest)
eval (Symbol "-" : os) (vars, x : y : rest) = eval os (vars, (y - x) : rest)
eval (Symbol "*" : os) (vars, x : y : rest) = eval os (vars, (y * x) : rest)
eval (Symbol "/" : _) (_, 0 : _ : _) = Left DivisionByZero
eval (Symbol "/" : os) (vars, x : y : rest) = eval os (vars, (y `div` x) : rest)
eval (Symbol s : _) (vars, _)
  | M.notMember s vars && not (isBuiltin s) = Left $ UnknownWord s
  | otherwise = Left StackUnderflow

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text = eval $ parseToken <$> T.words text

toList :: ForthState -> [Int]
toList = reverse . snd
