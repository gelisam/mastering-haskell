{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import System.IO

main :: IO ()
main = go newCounter
  where
    go :: Counter -> IO ()
    go c = do let pair = (increment c, increment c)
              (c1,c2) <- runParallel $ parPair pair
              printCounter c1
              printCounter c2
              go c1












data Counter = Counter { field1 :: !Int
                       , field2 :: !Int
                       }

newCounter :: Counter
newCounter = Counter 0 0

assertInvariant :: Counter -> Counter
assertInvariant c@(Counter m n) | m == n    = c
                                | otherwise = error "violation"

increment :: Counter -> Counter
increment = assertInvariant
          . (\(Counter m n) -> Counter (m+1) (n+1))
          . assertInvariant

printCounter :: Counter -> IO ()
printCounter (Counter _ n) = do
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
