{-# LANGUAGE OverloadedStrings #-}
module WordProblem (answer) where

import Control.Applicative ((<|>))
import qualified Data.Text as T

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

data Operator = Plus | Minus | Times | Divided

answer :: String -> Maybe Integer
answer s = A.maybeResult $ A.parse problem (T.pack s)

problem :: Parser Integer
problem = do
  _ <- question
  n <- number
  m <- A.option n $ binaryOp n
  o <- A.option m $ binaryOp m
  _ <- questionMark
  return o

number :: Parser Integer
number = A.signed A.decimal

binaryOp :: Integer -> Parser Integer
binaryOp a = do
  _ <- A.skipSpace
  op <- operator
  _ <- A.skipSpace
  b <- number
  return (case op of
    Plus -> a + b
    Minus -> a - b
    Times -> a * b
    Divided -> a `div` b)

operator :: Parser Operator
operator = plus <|> minus <|> times <|> divided
  where
    plus = A.asciiCI "plus" *> pure Plus
    minus = A.asciiCI "minus" *> pure Minus
    times = A.asciiCI "multiplied by" *> pure Times
    divided = A.asciiCI "divided by" *> pure Divided

question :: Parser ()
question = do
  _ <- A.string "What is "
  return ()

questionMark :: Parser ()
questionMark = do
  _ <- A.string "?"
  return ()
