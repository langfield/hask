module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n > 0 = Just $ primes !! (n - 1)
  | otherwise = Nothing


-- The first number is a prime, and then we sieve out all numbers divisible by
-- it. Thus the next number is a prime, and so on.
primes :: [Integer]
primes = sieve [2..]
  where sieve [] = []
        sieve (p : ps) = p : sieve [x | x <- ps, mod x p /= 0]
