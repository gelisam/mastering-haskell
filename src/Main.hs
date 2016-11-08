module Main where

import Text.Printf


fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


main :: IO ()
main = printf "fib 10 = %d" (fib 10)
