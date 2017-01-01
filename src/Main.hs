{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import System.IO.Unsafe

noisyPlus :: Num a => a -> a -> a
noisyPlus x y = traceThread "cache" $ x + y

cache :: [Integer]
cache = take 10 $ 1:1:zipWith noisyPlus cache (tail cache)

fib :: Int -> Integer
fib n | n <  10   = cache !! n
      | otherwise = traceThread "fib" $ fib (n-1) + fib (n-2)

main :: IO ()
main = traceThread "main" $ do
  r <- runParallel $ parPair (fib 11, fib 12)
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
    _ <- forkIO $ traceThread "thread"
                $ do !x <- ioX
                     putMVar varX x
    takeMVar varF <*> takeMVar varX

parPair :: (a, b) -> Parallel (a, b)
parPair (x, y) = (,) <$> pure x <*> pure y
