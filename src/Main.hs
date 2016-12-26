{-# LANGUAGE BangPatterns, GADTs, GeneralizedNewtypeDeriving #-}
module Main where
import Control.Concurrent

fib :: Int -> Tree Integer
fib 0 = pure 1
fib 1 = pure 1
fib n = (+) <$> fib (n-1)
            <*> fib (n-2)

fib10 :: Tree Integer
fib10 = (\x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82 x83 x84 x85 x86 x87 x88 x89 -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40 + x41 + x42 + x43 + x44 + x45 + x46 + x47 + x48 + x49 + x50 + x51 + x52 + x53 + x54 + x55 + x56 + x57 + x58 + x59 + x60 + x61 + x62 + x63 + x64 + x65 + x66 + x67 + x68 + x69 + x70 + x71 + x72 + x73 + x74 + x75 + x76 + x77 + x78 + x79 + x80 + x81 + x82 + x83 + x84 + x85 + x86 + x87 + x88 + x89)
    <$> pure 1  --    []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []
    <*> pure 1  -- ++ []

















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
