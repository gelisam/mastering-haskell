{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import Control.Monad
import System.IO.Unsafe

traceThread :: String -> a -> a
traceThread msg x = unsafePerformIO $ do
  threadId <- myThreadId
  putStrLn $ "[" ++ show threadId ++ "] " ++ msg
  return x

parMap :: (a -> b) -> [a] -> [b]
parMap f xs = unsafePerformIO $ do
  vars <- forM xs $ \x -> do
    var <- newEmptyMVar
    _ <- forkIO $ traceThread "thread" $ do let y = f x
                                            putMVar var y
    return var
  mapM readMVar vars

main :: IO ()
main = traceThread "main" $ do
  let r = parMap (fib . traceThread "fib") [10,20]
  print r



fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
