{-# LANGUAGE OverloadedStrings #-}
module Poker (bestHands) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)

import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Rank = Number Integer | Jack | Queen | King | Ace deriving Eq
data Suit = Clubs | Diamonds | Hearts | Spades deriving Eq
data Card = Card Rank Suit
newtype Hand = Hand [Card]

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

instance Show Hand where
  show (Hand cards) = (unwords . map show) cards

instance Eq Card where
  (==) (Card a _) (Card b _) = a == b

instance Ord Card where
  (<=) a b = scoreCardHigh a <= scoreCardHigh b

bestHands :: [String] -> Maybe [String]
bestHands xs = go <$> hands
  where
    hands = mapM (either (const Nothing) Just . Atto.parseOnly handParser . T.pack) xs

    go :: [Hand] -> [String]
    go hands' =
      case scoreHandAssocs of
        [] -> []
        ((_, hs) : _) -> map show hs
      where
        scoreHandAssocs = M.toDescList $ M.fromListWith (++) $ zip (map scoreHand hands') (map (\h -> [h]) hands')

scoreHand :: Hand -> Integer
scoreHand (Hand cards)
  | isStraight cards && isFlush cards = 9
  | L.maximum counts == 4 = 8
  | take 2 counts == [3, 2] = 7
  | otherwise = 0
  where
    counts = (L.reverse . L.sort . M.elems . M.fromListWith (+)) (zip cards (repeat (1 :: Integer)))

    isSame :: (Eq a) => [a] -> Bool
    isSame [] = True
    isSame (x:xs) = all (== x) xs

    cSuit :: Card -> Suit
    cSuit (Card _ suit) = suit

    isFlush :: [Card] -> Bool
    isFlush = isSame . map cSuit

    isStraight :: [Card] -> Bool
    isStraight cs =
      if lowScores == [0, 1, 2, 3, 4] || highScores == [0, 1, 2, 3, 4]
         then True
         else False
      where
        lowScores = (L.sort . rot 15 . map scoreCardLow) cs
        highScores = (L.sort . rot 15 . map scoreCardHigh) cs

    rot :: Integer -> [Integer] -> [Integer]
    rot _ [] = []
    rot n xs = map ((`mod` n) . (\k -> k - low)) xs
      where
        low = L.minimum xs

scoreCardHigh :: Card -> Integer
scoreCardHigh (Card (Number k) _) = k
scoreCardHigh (Card Jack _) = 11
scoreCardHigh (Card Queen _) = 12
scoreCardHigh (Card King _) = 13
scoreCardHigh (Card Ace _) = 14

scoreCardLow :: Card -> Integer
scoreCardLow (Card Ace _) = 1
scoreCardLow c = scoreCardHigh c


handParser :: Parser Hand
handParser = do
  cards <- Atto.count 5 cardParser
  if length cards == (length . S.fromList) cards then return (Hand cards) else fail "Hand has duplicate cards"

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
