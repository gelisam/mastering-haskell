{-# LANGUAGE BangPatterns #-}
module Main where
import Debug.Trace

foo :: Num a => [a] -> a
foo = go 0
  where
    go acc []     = acc
    go acc (x:xs) = do
      let acc' = trace "add" $ acc + x
      trace "recursive call" $ go acc' xs

main :: IO ()
main = print (foo [1..10] :: Int)
