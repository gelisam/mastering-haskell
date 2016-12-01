module Main where

type FList a = [a]
data IList a = Cons a (IList a)

appendF :: FList a -> FList a -> FList a
appendF []     xs' = xs'
appendF (x:xs) xs' = x : appendF xs xs'




















main :: IO ()
main = return ()
