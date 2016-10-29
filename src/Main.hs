module Main where

import Data.IORef
import System.IO.Unsafe







readGlobalCount :: IO Int
readGlobalCount = readIORef (unsafePerformIO $ newIORef 0)

incrGlobalCounter :: IO ()
incrGlobalCounter = modifyIORef (unsafePerformIO $ newIORef 0) (+ 1)


main :: IO ()
main = do
  putStrLn "example:"
  
  
  incrGlobalCounter
  incrGlobalCounter
  incrGlobalCounter
  print =<< readGlobalCount
