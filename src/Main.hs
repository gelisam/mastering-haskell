{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent









type Counter = MVar Int

newCounter :: IO Counter
newCounter = newMVar 0

increment :: Counter -> IO ()
increment counter = do
  n <- takeMVar counter
  let !n' = n + 1
  putMVar counter n'











main :: IO ()
main = return ()
