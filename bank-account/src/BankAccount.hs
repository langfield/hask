module BankAccount
  ( BankAccount
  , closeAccount
  , getBalance
  , incrementBalance
  , openAccount
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)

newtype BankAccount = BankAccount (MVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount m) = modifyMVar_ m (const (pure Nothing))

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount m) = readMVar m

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount m) n = modifyMVar m go
  where
    go :: Maybe Integer -> IO (Maybe Integer, Maybe Integer)
    go Nothing  = pure (Nothing, Nothing)
    go (Just x) = pure (Just (x + n), Just (x + n))

openAccount :: IO BankAccount
openAccount = BankAccount <$> newMVar (Just 0)
