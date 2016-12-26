{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Main where














parMap :: (a -> Parallel b) -> [a] -> Parallel [b]
parMap = undefined

parList :: [Parallel a] -> Parallel [a]
parList = undefined

parPair :: (Parallel a, Parallel b) -> Parallel (a,b)
parPair = undefined









newtype Parallel a = Parallel { runParallel :: IO a }
  deriving (Functor, Applicative)

main :: IO ()
main = return ()
