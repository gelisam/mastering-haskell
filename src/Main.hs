module Main where
import Control.Concurrent.Async
import Control.Exception.Base

fibPair :: (Integer, Integer)
fibPair = (fib 10, fib 20)

parPair :: (a, b) -> Concurrently (a, b)
parPair (x, y) = (,) <$> Concurrently (evaluate x)
                     <*> Concurrently (evaluate y)

main :: IO ()
main = do
  r <- runConcurrently $ parPair fibPair
  print r










fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
