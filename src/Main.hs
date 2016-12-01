module Main where

type FList a = [a]
data IList a = Cons a (IList a)

fmapF :: (a -> b) -> FList a -> FList b
fmapF _ []     = []
fmapF f (x:xs) = f x : fmapF f xs

fmapI :: (a -> b) -> IList a -> IList b
fmapI f (Cons x xs) = f x `Cons` fmapI f xs



















main :: IO ()
main = return ()
