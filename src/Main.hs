{-# LANGUAGE BangPatterns, GADTs, GeneralizedNewtypeDeriving #-}
module Main where
import Control.Concurrent

parallelTree :: Tree a -> Parallel a
parallelTree (Pure x)         = pure x
parallelTree (Ap ts (Sub te)) = parallelTree ts
                            <*> parallelTree te

sequentialTree :: Tree a -> a
sequentialTree (Pure x)         = x
sequentialTree (Ap ts (Sub te)) = sequentialTree ts
                                $ sequentialTree te

















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
