{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent

parOr :: Bool -> Bool -> IO Bool
parOr b1 b2 = raceM (return $ b1 || b2)
                    (return $ b2 || b1)

raceM :: IO a -> IO a -> IO a
raceM ioX1 ioX2 = do
  var <- newEmptyMVar
  _ <- forkIO $ do !x1 <- ioX1; putMVar var x1
  _ <- forkIO $ do !x2 <- ioX2; putMVar var x2
  readMVar var

main :: IO ()
main = do
  putStrLn "go!"
  print =<< parOr (fib 30 > 100) (fib 40 > 100)
  print =<< parOr (fib 40 > 100) (fib 30 > 100)








fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
