module GuessingGame
  ( reply
  ) where

reply :: Int -> String
reply 41 = "So close"
reply 42 = "Correct"
reply 43 = "So close"
reply guess = case status of
  LT -> "Too low"
  _ -> "Too high"
  where status = compare guess 42
