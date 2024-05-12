module RationalNumbers
(Rational,
 abs,
 numerator,
 denominator,
 add,
 sub,
 mul,
 div,
 pow,
 expRational,
 expReal,
 rational) where

import Prelude hiding (div, abs, Rational)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving (Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational (x, y) = Rational (sgn * x' `P.div` d) (y' `P.div` d)
  where
    d = gcd x y
    x' = P.abs x
    y' = P.abs y
    sgn = if x*y < 0 then -1 else 1

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational a b) = Rational (P.abs a) (P.abs b)

numerator :: Integral a => Rational a -> a
numerator (Rational a _) = a

denominator :: Integral a => Rational a -> a
denominator (Rational _ b) = b

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational a b) (Rational c d) = rational (a*d + c*b, b*d)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational a b) (Rational c d) = rational (a*d - c*b, b*d)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational a b) (Rational c d) = rational (a*c, b*d)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational a b) (Rational c d) = rational (a*d, b*c)

pow :: (Show a, Integral a) => Rational a -> a -> Rational a
pow (Rational a b) n =
  if n >= 0
    then rational (a ^ n, b ^ n)
    else rational (b ^ (-n), a ^ (-n))

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational a b) n = (fromIntegral a ** n) / (fromIntegral b ** n)

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal x (Rational a b) = x ** (fromIntegral a / fromIntegral b)
