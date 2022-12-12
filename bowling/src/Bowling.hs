module Bowling (score, BowlingError(..)) where

import Data.List (foldl')
import Debug.Trace (trace)

traceAs :: Show a => String -> a -> a
traceAs s x = trace (s ++ ": " ++ show x) x

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

-- Possibly rename Complete/Incomplete
data FrameStatus = Complete | Incomplete | Fill deriving (Eq, Show)

-- A possibly incomplete game.

data Roll = Strike | Ball Int deriving (Eq, Show)
type Total = Int
type RollIdx = Int
type FrameIdx = Int
data Game = Game Total RollIdx FrameIdx FrameStatus Roll Roll deriving Show

score :: [Int] -> Either BowlingError Int
score rolls =
  case errOrGame of
    Left err -> Left err
    Right (Game total _ 10 _ _ _) -> Right total
    Right Game {} -> Left IncompleteGame
  where
    errOrGame = getGame rolls


getGame :: [Int] -> Either BowlingError Game
getGame = foldl' f (Right $ Game 0 0 0 Complete (Ball 0) (Ball 0))
  where
    f :: Either BowlingError Game -> Int -> Either BowlingError Game
    f (Left err) _ = Left err
    f (Right g@(Game _ n m stat _ _)) x
      | m >= 10 = Left $ InvalidRoll n x
      | x > 10  = Left $ InvalidRoll n x
      | x < 0   = Left $ InvalidRoll n x
      | m == 9  = lastFrame x g
      | otherwise =
        case stat of
          Incomplete -> continueFrame x g
          Complete   -> newFrame x g
          _ -> Left $ InvalidRoll n x


lastFrame :: Int -> Game -> Either BowlingError Game
lastFrame x (Game total n _ Complete old older) = Right $ Game (total + pts) (n + 1) 9 Incomplete old' old
  where
    pts =
      case (old, older) of
        (Strike, Strike) -> 3 * x
        (_, Strike) -> 2 * x
        (Strike, _) -> 2 * x
        _ -> x
    old' = if x == 10 then Strike else Ball x
lastFrame x (Game total n _ Incomplete Strike older) = Right $ Game (total + pts) (n + 1) 9 Fill old' Strike
  where
    pts = if older == Strike then 2 * x else x
    old' = if x == 10 then Strike else Ball x
lastFrame x (Game total n _ Incomplete (Ball a) older)
  | x + a > 10  = Left $ InvalidRoll n x
  | x + a == 10 = Right $ Game (total + pts) (n + 1) 9 Fill old' (Ball a)
  | x + a < 10  = Right $ Game (total + pts) (n + 1) 10 Complete old' (Ball a)
  | otherwise   = Left $ InvalidRoll n x
  where
    pts = if older == Strike then 2 * x else x
    old' = if x == 10 then Strike else Ball x
lastFrame x (Game total n _ Fill Strike _) = Right $ Game (total + x) (n + 1) 10 Complete old' Strike
  where
    old' = if x == 10 then Strike else Ball x
lastFrame x (Game total n _ Fill (Ball a) (Ball b))
  | a + b == 10 = Right $ Game (total + x) (n + 1) 10 Complete old' (Ball a)
  | otherwise = Left $ InvalidRoll n x
  where
    old' = if x == 10 then Strike else Ball x
lastFrame x (Game total n _ Fill (Ball a) Strike)
  | a + x <= 10 = Right $ Game (total + x) (n + 1) 10 Complete old' (Ball a)
  | otherwise = Left $ InvalidRoll n x
  where
    old' = if x == 10 then Strike else Ball x



continueFrame :: Int -> Game -> Either BowlingError Game
continueFrame x (Game total n m _ old older) =
  case old of
    Strike -> Right $ Game (total + pts) (n + 1) (m + 1) Complete old' old
    Ball a -> if x + a > 10 
              then Left $ InvalidRoll n x
              else Right $ Game (total + pts) (n + 1) (m + 1) Complete old' old
  where
    pts :: Int
    pts
      | older == Strike = 2 * x
      | otherwise       = x
    old' = if x == 10 then Strike else Ball x


newFrame :: Int -> Game -> Either BowlingError Game
newFrame x (Game total n m _ old older) = Right $ Game (total + pts) (n + 1) m' stat' old' old
  where
    pts =
      case (old, older) of
        (Strike, Strike) -> 3 * x
        (Strike, _)      -> 2 * x
        (_, Strike)      -> 2 * x
        (Ball a, Ball b) ->
          case a + b of
            10 -> 2 * x
            _  -> x
    pair :: (Int, FrameStatus)
    pair
      | x == 10 = (m + 1, Complete)
      | otherwise = (m, Incomplete)
    (m', stat') = pair
    old' = if x == 10 then Strike else Ball x
