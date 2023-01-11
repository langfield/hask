module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, put)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import System.Random (randomRIO)

import qualified Data.Set as S

newtype Robot = Robot (IORef String)
type RunState = Set String

iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
  | p v = return v
  | otherwise = f v >>= iterateUntilM p f

initialState :: RunState
initialState = S.empty

sample :: String -> StateT RunState IO String
sample _ = do
  letters <- replicateM 2 $ randomRIO ('A','Z') 
  digits <- replicateM 3 $ randomRIO ('0','9')
  pure $ letters ++ digits

mkName :: StateT RunState IO String
mkName = do
  names <- get
  initial <- sample ""
  name <- iterateUntilM (`S.notMember` names) sample initial
  put (S.insert name names)
  pure name

mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- mkName
  nameRef <- liftIO $ newIORef name
  pure $ Robot nameRef

resetName :: Robot -> StateT RunState IO ()
resetName (Robot nameRef) = do
  name <- mkName
  liftIO $ writeIORef nameRef name

robotName :: Robot -> IO String
robotName (Robot nameRef) = do
  liftIO $ readIORef nameRef
