module Main where
import Control.Concurrent.Async
import Control.Exception.Base

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parOr :: Bool -> Bool -> IO Bool
parOr b1 b2 = either id id <$> race (evaluate $ b1 || b2)
                                    (evaluate $ b2 || b1)

main :: IO ()
main = do print =<< parOr (fib 30 > 100) (fib 40 > 100)
          print =<< parOr (fib 40 > 100) (fib 30 > 100)
