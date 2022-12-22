{-# LANGUAGE OverloadedStrings #-}

module Poker (bestHands) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import Data.Map (Map)
import GHC.Data.Maybe (rightToMaybe)

import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

type High = Card
type Quad = Card
type Kicker = Card
type HighKicker = Card
type MiddleKicker = Card
type LowKicker = Card
type Triplet = Card
type Pair = Card
type HighPair = Card
type LowPair = Card

data Category
    = StraightFlush High
    | FourOfAKind Quad Kicker
    | FullHouse Triplet Pair
    | Flush
    | Straight High
    | ThreeOfAKind Triplet HighKicker LowKicker
    | TwoPair HighPair LowPair Kicker
    | OnePair Pair HighKicker MiddleKicker LowKicker
    | HighCard
    deriving (Eq)
data Rank = Number Integer | Jack | Queen | King | Ace deriving (Eq)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq)
data Card = Card Rank Suit
data Hand = Hand Category [Card]

instance Show Rank where
  show (Number k) = show k
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

instance Show Suit where
  show Clubs    = "C"
  show Diamonds = "D"
  show Hearts   = "H"
  show Spades   = "S"

instance Show Card where
  show (Card rank suit) = show rank ++ show suit

-- The issue with this is that if we want the tuple sorted for ease-of-access
-- to high cards, we lose the ordering of the hand, which means our output will
-- be wrong.
instance Show Hand where
  show (Hand _ cards) = (unwords . map show) cards

instance Eq Card where
  (==) (Card a _) (Card b _) = a == b

instance Ord Card where
  (<=) a b = scoreCardHigh a <= scoreCardHigh b

instance Eq Hand where
  (Hand (StraightFlush h) _) == (Hand (StraightFlush h') _) = h == h'
  (Hand (FourOfAKind q k) _) == (Hand (FourOfAKind q' k') _) =
    q == q' && k == k'
  (Hand (FullHouse t p) _) == (Hand (FullHouse t' p') _) = t == t' && p == p'
  (Hand Flush cs) == (Hand Flush cs') = L.sort cs == L.sort cs'
  (Hand (Straight h) _) == (Hand (Straight h') _) = h == h'
  (Hand (ThreeOfAKind t h l) _) == (Hand (ThreeOfAKind t' h' l') _) =
    t == t' && h == h' && l == l'
  (Hand (TwoPair h l k) _) == (Hand (TwoPair h' l' k') _) =
    h == h' && l == l' && k == k'
  (Hand (OnePair p h m l) _) == (Hand (OnePair p' h' m' l') _) =
    p == p' && h == h' && m == m' && l == l'
  (Hand HighCard cs) == (Hand HighCard cs') = L.sort cs == L.sort cs'
  _ == _ = False

instance Ord Hand where
  (Hand (StraightFlush h) _) <= (Hand (StraightFlush h') _) = h <= h'
  (Hand (StraightFlush _) _) <= _ = False
  (Hand (FourOfAKind q k) _) <= (Hand (FourOfAKind q' k') _)
    | q == q' && k <= k' = True
    | q < q' = True
    | otherwise = False
  (Hand (FourOfAKind _ _) _) <= _ = False
  (Hand (FullHouse t p) _) <= (Hand (FullHouse t' p') _)
    | t == t' && p <= p' = True
    | t < t' = True
    | otherwise = False
  (Hand (FullHouse _ _) _) <= _ = False
  (Hand Flush cs) <= (Hand Flush cs') = (reverse . L.sort) cs <= (reverse . L.sort) cs'
  (Hand Flush _) <= _ = False
  (Hand (Straight h) _) <= (Hand (Straight h') _) = h <= h'
  (Hand (Straight _) _) <= _ = False
  (Hand (ThreeOfAKind t h l) _) <= (Hand (ThreeOfAKind t' h' l') _)
    | t < t' = True
    | t == t' && h < h' = True
    | t == t' && h == h' && l <= l' = True
    | otherwise = False
  (Hand (ThreeOfAKind _ _ _) _) <= _ = False
  (Hand (TwoPair h l k) _) <= (Hand (TwoPair h' l' k') _)
    | h < h' = True
    | h == h' && l < l' = True
    | h == h' && l == l' && k <= k' = True
    | otherwise = False
  (Hand (TwoPair _ _ _) _) <= _ = False
  (Hand (OnePair p h m l) _) <= (Hand (OnePair p' h' m' l') _)
    | p < p' = True
    | p == p' && h < h' = True
    | p == p' && h == h' && m < m' = True
    | p == p' && h == h' && m == m' && l <= l' = True
    | otherwise = False
  (Hand (OnePair _ _ _ _) _) <= _ = False
  (Hand HighCard cs) <= (Hand HighCard cs') = (reverse . L.sort) cs <= (reverse . L.sort) cs'
  _ <= _ = False
  

bestHands :: [String] -> Maybe [String]
bestHands xs = 
  case eitherHands of
    Left msg -> Just [msg]
    Right hands -> (Just . map show . best) hands
  where
    eitherHands = (mapM (Atto.parseOnly handParser . T.pack)) xs

best :: [Hand] -> [Hand]
best hs = case (reverse . L.sort) hs of
  [] -> []
  (i : is) -> i : takeWhile (== i) is

scoreCardHigh :: Card -> Integer
scoreCardHigh (Card (Number k) _) = k
scoreCardHigh (Card Jack  _) = 11
scoreCardHigh (Card Queen _) = 12
scoreCardHigh (Card King  _) = 13
scoreCardHigh (Card Ace   _) = 14

scoreCardLow :: Card -> Integer
scoreCardLow (Card Ace _) = 1
scoreCardLow c = scoreCardHigh c

handParser :: Parser Hand
handParser = do
  cs <- Atto.count 5 cardParser
  if isUnique cs then handParser' cs else fail "Hand has duplicate cards"
  where
    isUnique :: Ord a => [a] -> Bool
    isUnique xs = length xs == (length . S.fromList) xs

handParser' :: [Card] -> Parser Hand
handParser' cs
  | length cs /= 5 = fail "Hand does not have 5 cards"
  | otherwise = case (isStraight cs, isFlush cs, countAssocs) of
    (True, True, _) -> pure $ Hand (StraightFlush high) cs
    (_, _, (4, [q]) : (1, [k]) : _) -> pure $ Hand (FourOfAKind q k) cs
    (_, _, (3, [t]) : (2, [p]) : _) -> pure $ Hand (FullHouse t p) cs
    (_, True, _) -> pure $ Hand Flush cs
    (True, _, _) -> pure $ Hand (Straight (L.maximum cs)) cs
    (_, _, (3, [t]) : (1, [l, h]) : _) -> pure $ Hand (ThreeOfAKind t h l) cs
    (_, _, (2, [l, h]) : (1, [k]) : _) -> pure $ Hand (TwoPair h l k) cs
    (_, _, (2, [p]) : (1, [l, m, h]) : _) -> pure $ Hand (OnePair p h m l) cs
    _ -> pure $ Hand HighCard cs
  where
    high = L.maximum cs
    countAssocs = (M.toDescList . M.map L.sort . invert . M.fromListWith (+))
      (zip cs (repeat (1 :: Integer)))

invert :: Ord v => Map k v -> Map v [k]
invert m = M.fromListWith (++) pairs
  where pairs = [ (v, [k]) | (k, v) <- M.toList m ]

isSame :: (Eq a) => [a] -> Bool
isSame [] = True
isSame (x : xs) = all (== x) xs

cSuit :: Card -> Suit
cSuit (Card _ suit) = suit

isFlush :: [Card] -> Bool
isFlush = isSame . map cSuit

isStraight :: [Card] -> Bool
isStraight cs = lowScores == [0, 1, 2, 3, 4] || highScores == [0, 1, 2, 3, 4]
  where
    lowScores  = (L.sort . rot 15 . map scoreCardLow) cs
    highScores = (L.sort . rot 15 . map scoreCardHigh) cs

rot :: Integer -> [Integer] -> [Integer]
rot _ [] = []
rot n xs = map ((`mod` n) . (\k -> k - low)) xs where low = L.minimum xs

cardParser :: Parser Card
cardParser = do
  rank <- rankParser
  suit <- suitParser
  Atto.skipSpace
  return (Card rank suit)

rankParser :: Parser Rank
rankParser = do
  rankNumParser <|> rankFaceParser
  where
    rankNumParser :: Parser Rank
    rankNumParser = Number <$> Atto.decimal

    rankFaceParser :: Parser Rank
    rankFaceParser =
      (Atto.string "J" >> return Jack)
        <|> (Atto.string "Q" >> return Queen)
        <|> (Atto.string "K" >> return King)
        <|> (Atto.string "A" >> return Ace)

suitParser :: Parser Suit
suitParser =
  (Atto.string "C" >> return Clubs)
    <|> (Atto.string "D" >> return Diamonds)
    <|> (Atto.string "H" >> return Hearts)
    <|> (Atto.string "S" >> return Spades)
