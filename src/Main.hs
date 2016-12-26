{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Main where
import Control.Concurrent

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


fib :: Int -> Parallel Integer
fib 0 = pure 1
fib 1 = pure 1
fib n = (+) <$> fib (n-1) <*> fib (n-2)

main :: IO ()
main = do
  r <- runParallel $ traverse fib [0..25]
  print r









newtype Parallel a = Parallel { runParallel :: IO a }
  deriving (Functor)
