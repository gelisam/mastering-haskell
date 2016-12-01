module Main where

type FList a = [a]
data IList a = Cons a (IList a)

filterF :: (a -> Bool) -> FList a -> FList a
filterF _ []     = []
filterF p (x:xs) | p x       = x : filterF p xs
                 | otherwise =     filterF p xs

filterI :: (a -> Bool) -> IList a -> IList a
filterI p (Cons x xs) | p x       = x `Cons` filterI p xs
                      | otherwise =          filterI p xs



















main :: IO ()
main = return ()
