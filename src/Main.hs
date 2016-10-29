module Main where 

import Data.IORef
import System.IO.Unsafe



{-# NOINLINE globalCounter #-}
globalCounter :: IORef Int
globalCounter = unsafePerformIO $ newIORef 0

readGlobalCount :: Int
readGlobalCount = unsafePerformIO $ readIORef globalCounter


main :: IO ()
main = do
  putStrLn "example:"
  
  let x = readGlobalCount
  writeIORef globalCounter (x + 1)
  print readGlobalCount
