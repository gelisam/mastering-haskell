{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Main where


instance Applicative Parallel where
  pure = Parallel . pure
  Parallel ioF <*> Parallel ioX = Parallel $ do
    f <- ioF
    x <- ioX
    return (f x)






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
