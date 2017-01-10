module Main where
import Control.Monad


fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = forM_ [10,20,30,40] $ \n -> do
  print (fib n > 100)
