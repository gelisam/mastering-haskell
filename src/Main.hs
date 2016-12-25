{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Main where














parMap :: (a -> Parallel b) -> [a] -> Parallel [b]
parMap = traverse

parList :: [Parallel a] -> Parallel [a]
parList = sequenceA

parPair :: (Parallel a, Parallel b) -> Parallel (a, b)
parPair (px, py) = (,) <$> px <*> py









newtype Parallel a = Parallel { runParallel :: IO a }
  deriving (Functor, Applicative)

main :: IO ()
main = return ()
