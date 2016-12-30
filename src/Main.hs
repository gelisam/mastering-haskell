{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import System.IO.Unsafe

fibPair :: (Integer, Integer)
fibPair = (fib 10, fib 20)

parPair :: (a, b) -> Parallel (a, b)
parPair (x, y) = ((,) <$> pure x) <*> pure y











main :: IO ()
main = traceThread "main" $ do
  r <- runParallel $ parPair fibPair
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
  fmap f (Parallel ioX) = Parallel (fmap f ioX)

instance Applicative Parallel where
  pure = Parallel . pure
  Parallel ioF <*> Parallel ioX = Parallel $ do
    varF <- newEmptyMVar
    varX <- newEmptyMVar
    _ <- forkIO $ traceThread "thread"
                $ do !f <- ioF
                     putMVar varF f
    _ <- forkIO $ traceThread "thread"
                $ do !x <- ioX
                     putMVar varX x
    takeMVar varF <*> takeMVar varX
