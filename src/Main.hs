{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import System.IO
import System.IO.Unsafe

main :: IO ()
main = do
  counter <- newCounter
  let sharedThunk :: Int
      sharedThunk = unsafePerformIO $ do
        increment counter
        return 42
  let !_ = sharedThunk + sharedThunk
  printCounter counter
  main

increment :: Counter -> IO ()
increment counter = do
  n <- takeMVar counter
  let !n' = n + 1
  putMVar counter n'










type Counter = MVar Int

newCounter :: IO Counter
newCounter = newMVar 0

printCounter :: Counter -> IO ()
printCounter counter = do
  n <- takeMVar counter
  putStr $ show n ++ ", "
  hFlush stdout
  threadDelay (200 * 1000)



newtype Parallel a = Parallel { runParallel :: IO a }

instance Functor Parallel where
  fmap f (Parallel ioX) = Parallel $ do !x <- ioX
                                        return (f x)

instance Applicative Parallel where
  pure = Parallel . pure
  Parallel ioF <*> Parallel ioX = Parallel $ do
    varF <- newEmptyMVar
    varX <- newEmptyMVar
    _ <- forkIO $ do !f <- ioF
                     putMVar varF f
    _ <- forkIO $ do !x <- ioX
                     putMVar varX x
    takeMVar varF <*> takeMVar varX

parPair :: (a, b) -> Parallel (a, b)
parPair (x, y) = (,) <$> pure x <*> pure y
