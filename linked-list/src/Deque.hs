module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

newtype Deque a = Deque (IORef [a])

mkDeque :: IO (Deque a)
mkDeque = Deque <$> newIORef []

pop :: Deque a -> IO (Maybe a)
pop (Deque state) = do
  xs <- readIORef state
  case reverse xs of
    [] -> pure Nothing
    (y:ys) -> do
      writeIORef state ys
      pure (Just y)

push :: Deque a -> a -> IO ()
push (Deque state) x = do
  modifyIORef state (++ [x])

unshift :: Deque a -> a -> IO ()
unshift (Deque state) x = do
  modifyIORef state (x:)

shift :: Deque a -> IO (Maybe a)
shift (Deque state) = do
  xs <- readIORef state
  case xs of
    [] -> pure Nothing
    (y:ys) -> do
      writeIORef state ys
      pure (Just y)
