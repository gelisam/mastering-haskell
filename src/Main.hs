{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import System.IO.Unsafe

fib :: Int -> Integer
fib n | n <= 1    = 1
      | otherwise = x1 + x2
  where
    x1 = traceThread "fib (n-1)" $ fib (n-1)
    x2 = traceThread "fib (n-2)" $ fib (n-2)

parInteger :: Integer -> Parallel Integer
parInteger = pure







main :: IO ()
main = traceThread "main" $ do
  r <- runParallel $ parInteger $ fib 2
  print r










traceThread :: String -> a -> a
traceThread msg x = unsafePerformIO $ do
  threadId <- myThreadId
  putStrLn $ "[" ++ show threadId ++ "] " ++ msg
  return x



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
