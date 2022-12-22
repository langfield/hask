{-# LANGUAGE OverloadedStrings #-}

module Poker (bestHands) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import GHC.Data.Maybe (rightToMaybe)

import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Category
    = StraightFlush
    | FourOfAKind
    | FullHouse
    | Flush
    | Straight
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard deriving Eq
data Rank = Number Integer | Jack | Queen | King | Ace deriving (Eq)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq)
data Card = Card Rank Suit
data Hand = Hand Category (Card, Card, Card, Card, Card)

getCardList :: (Card, Card, Card, Card, Card) -> [Card]
getCardList (c1, c2, c3, c4, c5) = [c1, c2, c3, c4, c5]

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
  show (Hand _ cards) = (unwords . map show . getCardList) cards

instance Eq Card where
  (==) (Card a _) (Card b _) = a == b

instance Ord Card where
  (<=) a b = scoreCardHigh a <= scoreCardHigh b

instance Eq Hand where
  (Hand cat1 (a1, _, _, _, _)) == (Hand cat2 (a2, _, _, _, _)) =
    cat1 == cat2 && a1 == a2

instance Ord Hand where
  (Hand StraightFlush a) <= (Hand StraightFlush b) = a <= b
  (Hand StraightFlush _) <= _ = False
  (Hand FourOfAKind a) <= (Hand FourOfAKind b) = a <= b
  (Hand FourOfAKind _) <= _ = False
  (Hand FullHouse a) <= (Hand FullHouse b) = a <= b
  (Hand FullHouse _) <= _ = False

bestHands :: [String] -> Maybe [String]
bestHands xs = map show . best <$> maybeHands
  where
    maybeHands = (rightToMaybe . mapM (Atto.parseOnly handParser . T.pack)) xs


best :: [Hand] -> [Hand]
best hs = case L.sort hs of
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
  if isUnique cs
    then case cs of
      [c1, c2, c3, c4, c5] -> handParser' (c1, c2, c3, c4, c5)
    else fail "Hand has duplicate cards"
  where
    isUnique :: Ord a => [a] -> Bool
    isUnique xs = length xs == (length . S.fromList) xs

handParser' :: (Card, Card, Card, Card, Card) -> Parser Hand
handParser' cards
  | isStraight cs && isFlush cs = pure $ Hand StraightFlush cards
  | L.maximum counts == 4 = pure $ Hand FourOfAKind cards
  | take 2 counts == [3, 2] = pure $ Hand FullHouse cards
  | isFlush cs    = pure $ Hand Flush cards
  | isStraight cs = pure $ Hand Straight cards
  | L.maximum counts == 3 = pure $ Hand ThreeOfAKind cards
  | take 2 counts == [2, 2] = pure $ Hand TwoPair cards
  | take 1 counts == [2] = pure $ Hand OnePair cards
  | otherwise     = pure $ Hand HighCard cards
  where
    cs     = getCardList cards
    counts = (L.reverse . L.sort . M.elems . M.fromListWith (+))
      (zip cs (repeat (1 :: Integer)))

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
