module BankAccount
  ( BankAccount
  , closeAccount
  , getBalance
  , incrementBalance
  , openAccount
  ) where

data Status = Open | Closed
data BankAccount = BankAccount Integer Status

closeAccount :: BankAccount -> IO ()
closeAccount _ = pure ()

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount x Open  ) = pure $ Just x
getBalance (BankAccount _ Closed) = pure Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount _ Closed) _ = pure Nothing
incrementBalance (BankAccount x Open  ) y = pure $ Just $ x + y

openAccount :: IO BankAccount
openAccount = pure $ BankAccount 0 Open
