{-# LANGUAGE BangPatterns #-}
module Main where
import Data.List

data Behaviour a = Branch !a (Behaviour a) (Behaviour a)
type Event     a = Behaviour [a]

mergeE :: Event a -> Event a -> Event a
mergeE e1 e2 = (++) <$> e1 <*> e2

holdB :: a -> Event a -> Behaviour a
holdB x (Branch xs xss1 xss2) = let x' = last (x:xs)
                                in Branch x' (holdB x' xss1)
                                             (holdB x' xss2)
scanE :: (b -> a -> b) -> b -> Event a -> Event b
scanE f y (Branch xs xss1 xss2) = let ys = scanl' f y xs
                                      y' = last ys
                                   in Branch (tail ys)
                                             (scanE f y' xss1)
                                             (scanE f y' xss2)
















instance Functor Behaviour where
  fmap f (Branch x xs1 xs2) = Branch (f x)
                                     (fmap f xs1)
                                     (fmap f xs2)

instance Applicative Behaviour where
  pure x = Branch x (pure x) (pure x)
  Branch f fs1 fs2 <*> Branch x xs1 xs2 = Branch (f x)
                                                 (fs1 <*> xs1)
                                                 (fs2 <*> xs2)





main :: IO ()
main = return()
