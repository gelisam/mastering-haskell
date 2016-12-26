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


parMap :: (a -> Parallel b) -> [a] -> Parallel [b]
parMap = traverse

parList :: [Parallel a] -> Parallel [a]
parList = sequenceA

parPair :: (Parallel a, Parallel b) -> Parallel (a, b)
parPair (px, py) = (,) <$> px <*> py









newtype Parallel a = Parallel { runParallel :: IO a }
  deriving (Functor)

main :: IO ()
main = return ()
