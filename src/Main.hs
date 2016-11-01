module Main where

import Data.IORef








readGlobalCount :: IORef Int -> IO Int
readGlobalCount globalCounter = readIORef globalCounter

incrGlobalCounter :: IORef Int -> IO ()
incrGlobalCounter globalCounter = modifyIORef globalCounter (+ 1)


main :: IO ()
main = do
  putStrLn "example:"
  
  globalCounter <- newIORef 0
  incrGlobalCounter globalCounter
  incrGlobalCounter globalCounter
  incrGlobalCounter globalCounter
  print =<< readGlobalCount globalCounter
