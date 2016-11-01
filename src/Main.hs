module Main where

import Data.IORef
import System.IO.Unsafe



{-# NOINLINE globalCounter #-}
globalCounter :: IORef Int
globalCounter = unsafePerformIO $ newIORef 0

readGlobalCount :: IO Int
readGlobalCount = readIORef globalCounter

incrGlobalCounter :: IO ()
incrGlobalCounter = modifyIORef globalCounter (+ 1)


main :: IO ()
main = do
  putStrLn "example:"
  
  
  incrGlobalCounter
  incrGlobalCounter
  incrGlobalCounter
  print =<< readGlobalCount
