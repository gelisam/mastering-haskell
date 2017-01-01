{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.IO

main :: IO ()
main = do
  counter <- newCounter
  forever $ do increment counter
               printCounter counter

printCounter :: Counter -> IO ()
printCounter (Counter {..}) = do
  n <- readIORef field1
  putStr $ show n ++ ", "
  hFlush stdout
  threadDelay (200 * 1000)







data Counter = Counter
  { field1 :: IORef Int
  , field2 :: IORef Int
  }

newCounter :: IO Counter
newCounter = Counter <$> newIORef 0 <*> newIORef 0

assertInvariant :: Counter -> IO ()
assertInvariant (Counter {..}) = do r <- (==) <$> readIORef field1
                                              <*> readIORef field2
                                    when (not r) $ fail "violation"

increment :: Counter -> IO ()
increment c@(Counter {..}) = do assertInvariant c
                                modifyIORef' field1 (+1)
                                modifyIORef' field2 (+1)
                                assertInvariant c
