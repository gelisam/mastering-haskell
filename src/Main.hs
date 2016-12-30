{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main where
import Control.Concurrent
import System.IO.Unsafe

fib :: Trie -> Int -> Integer
fib trie n | n <= 1    = 1
           | otherwise = (+) <$!> x1 <*?> x2
  where
    infixl 4 <*?>
    (<*?>) = current trie
    x1 = traceThread "fib (n-1)" $ fib (left  trie) (n-1)
    x2 = traceThread "fib (n-2)" $ fib (right trie) (n-2)

data Trie = Trie { left :: Trie, current :: Apply, right :: Trie }

sequential :: Trie
sequential = Trie sequential ($) sequential

upToDepth :: Int -> Trie
upToDepth 0 = sequential
upToDepth d = Trie (upToDepth (d-1)) (<*!>) (upToDepth (d-1))

main :: IO ()
main = traceThread "main" $ print $ fib (upToDepth 2) 5











type Apply = forall a b. (a -> b) -> a -> b

infixl 4 <$!>
infixl 4 <*!>

(<$!>) :: Apply
(<$!>) = ($!)

(<*!>) :: Apply
(<*!>) f x = unsafePerformIO $ do
  runParallel $ pure f <*> pure x



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
