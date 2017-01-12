{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent

fibPair :: (Integer, Integer)
fibPair = (fib 10, fib 20)

parPair :: (a, b) -> Parallel (a, b)
parPair (x, y) = (,) <$> pure x
                     <*> pure y

main :: IO ()
main = do
  r <- runParallel $ parPair fibPair
  print r










fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)



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
