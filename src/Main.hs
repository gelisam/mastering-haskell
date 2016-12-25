{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import Control.Monad
import System.IO.Unsafe



main :: IO ()
main = print $ parMap fib [0..25]

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parMap :: (a -> b) -> [a] -> [b]
parMap f xs = unsafePerformIO $ do
  vars <- forM xs $ \x -> do
    var <- newEmptyMVar
    _ <- forkIO $ do let !y = f x
                     putMVar var y
    return var
  mapM readMVar vars
