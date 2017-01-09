module Main where
import Prelude hiding ((||))

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || b = b

main :: IO ()
main = do
  putStrLn "go!"
  print $ (fib 30 > 100) || (fib 40 > 100)
  print $ (fib 40 > 100) || (fib 30 > 100)
















fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
