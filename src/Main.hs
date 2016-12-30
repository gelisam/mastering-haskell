{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import System.IO.Unsafe

fibList :: [Integer]
fibList = map fib [1..3]

parList :: [a] -> Parallel [a]
parList = traverse pure











main :: IO ()
main = traceThread "main" $ do
  r <- runParallel $ parList fibList
  print r










traceThread :: String -> a -> a
traceThread msg x = unsafePerformIO $ do
  threadId <- myThreadId
  putStrLn $ "[" ++ show threadId ++ "] " ++ msg
  return x

fib :: Int -> Integer
fib x = traceThread "fib" (go x)
  where
    go 0 = 1
    go 1 = 1
    go n = go (n-1) + go (n-2)



newtype Parallel a = Parallel { runParallel :: IO a }

instance Functor Parallel where
  fmap f (Parallel ioX) = Parallel $ do !x <- ioX
                                        return (f x)

instance Applicative Parallel where
  pure = Parallel . pure
  Parallel ioF <*> Parallel ioX = Parallel $ do
    varF <- newEmptyMVar
    varX <- newEmptyMVar
    _ <- forkIO $ traceThread "thread"
                $ do !f <- ioF
                     putMVar varF f
    _ <- forkIO $ do !x <- ioX
                     putMVar varX x
    takeMVar varF <*> takeMVar varX
