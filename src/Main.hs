module Main where

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
  putStrLn "go!"
  print $ fib 10
  print $ fib 20
  print $ fib 30
  print $ fib 40
