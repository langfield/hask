module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- | Count number of occurrences of a given element in a list of lists.
count :: Eq a => a -> [[a]] -> Int
count x = length . filter (== x) . concat

-- | Count number of tic-tac-toe wins for an element.
countWins :: Eq a => a -> [[a]] -> Int
countWins x board = nrows + ncols + diag1 + diag2
  where
    nrows = length $ filter (all (== x)) board
    ncols = length $ filter (all (== x)) $ transpose board
    diag1 = fromEnum $ all (== x) $ concatMap (take 1) $ zipWith drop [0..] board
    diag2 = fromEnum $ all (== x) $ concatMap (take 1) $ zipWith drop [0..] $ map reverse board

-- | Compute tic-tac-toe game state.
gameState :: [String] -> GameState
gameState board
  | (xcount, ocount) == (5,4) && (xwins,owins) == (0,0) = Draw
  | xcount == ocount + 1 && xwins > 0 && owins == 0 = WinX
  | xcount == ocount && owins > 0 && xwins == 0 = WinO
  | (xcount - ocount) `elem` [0,1] && (xwins,owins) == (0,0) = Ongoing
  | otherwise = Impossible
  where
    xcount = count 'X' board
    ocount = count 'O' board
    xwins = countWins 'X' board
    owins = countWins 'O' board
