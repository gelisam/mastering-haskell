{-# LANGUAGE BangPatterns, GADTs, GeneralizedNewtypeDeriving #-}
module Main where
import Control.Concurrent

fib :: Int -> Tree Integer
fib 0 = pure 1
fib 1 = pure 1
fib n = (+) <$> sub (fib (n-1))
            <*> sub (fib (n-2))

fib10 :: Tree Integer
fib10 = (+) <$> sub ((+) <$> sub (fib 8) <*> sub (fib 7))
            <*> sub ((+) <$> sub (fib 7) <*> sub (fib 6))


















data FreeAp f a where
  Pure :: a -> FreeAp f a
  Ap   :: FreeAp f (e -> a) -> f e -> FreeAp f a

instance Functor (FreeAp f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Ap fs fe) = Ap (fmap (fmap f) fs) fe

instance Applicative (FreeAp f) where
  pure = Pure
  Pure f   <*> fx = fmap f fx
  Ap fs fe <*> fx = Ap (flip <$> fs <*> fx) fe


data TreeF a = Sub (Tree a)
type Tree a = FreeAp TreeF a

sub :: Tree a -> Tree a
sub px = Ap (Pure id) (Sub px)


newtype Parallel a = Parallel { runParallel :: IO a }
  deriving (Functor)

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


main :: IO ()
main = return ()
