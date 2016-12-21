module Main where






main :: IO ()
main = print $ map fib [0..39]

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
