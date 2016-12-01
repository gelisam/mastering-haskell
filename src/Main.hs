module Main where

type FList a = [a]
data IList a = Cons a (IList a)

zipF :: FList a -> FList b -> FList (a,b)
zipF []     _      = []
zipF _      []     = []
zipF (x:xs) (y:ys) = (x,y) : zipF xs ys

zipI :: IList a -> IList b -> IList (a,b)
zipI (Cons x xs) (Cons y ys) = (x,y) `Cons` zipI xs ys



















main :: IO ()
main = return ()
