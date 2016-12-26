{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Main where
import Control.Concurrent
import Control.Monad

newtype Parallel a = Parallel { runParallel :: IO a }
  deriving (Functor, Applicative)

main :: IO ()
main = runParallel (parMap (pure . fib) [0..25]) >>= print

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parMap :: (a -> Parallel b) -> [a] -> Parallel [b]
parMap f xs = Parallel $ do
  vars <- forM xs $ \x -> do
    var <- newEmptyMVar
    _ <- forkIO $ do !y <- runParallel (f x)
                     putMVar var y
    return var
  mapM readMVar vars
