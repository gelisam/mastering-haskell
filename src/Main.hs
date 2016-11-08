module Main where

import Text.Printf


fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib n = fibs !! n


main :: IO ()
main = printf "fib 10 = %d" (fib 10)
