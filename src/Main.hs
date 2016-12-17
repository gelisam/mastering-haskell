{-# LANGUAGE BangPatterns #-}
module Main where
import Data.List

data Behaviour a = Cons !a (Behaviour a)
type Event     a = Behaviour [a]

mergeE :: Event a -> Event a -> Event a
mergeE e1 e2 = (++) <$> e1 <*> e2

holdB :: a -> Event a -> Behaviour a
holdB x (Cons xs xss) = let x' = last (x:xs)
                        in Cons x' (holdB x' xss)

scanE :: (b -> a -> b) -> b -> Event a -> Event b
scanE f y (Cons xs xss) = let ys = scanl' f y xs
                              y' = last ys
                          in Cons (tail ys) (scanE f y' xss)
















instance Functor Behaviour where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Behaviour where
  pure x = Cons x (pure x)
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)





main :: IO ()
main = return()
