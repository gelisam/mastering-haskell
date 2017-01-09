{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent

parOr :: Bool -> Bool -> IO Bool
parOr b1 b2 = raceI (return $ b1 || b2)
                    (return $ b2 || b1)

raceI :: IO a -> IO a -> IO a
raceI ioX1 ioX2 = do
  var <- newIVar
  _ <- forkIO $ do !x1 <- ioX1; putIVar var x1
  _ <- forkIO $ do !x2 <- ioX2; putIVar var x2
  readIVar var

main :: IO ()
main = do
  putStrLn "go!"
  print =<< parOr (fib 30 > 100) (fib 40 > 100)
  print =<< parOr (fib 40 > 100) (fib 30 > 100)
  
  
  
  
  
  
  
  
  
  
  
  let loop = do threadDelay (1000 * 1000)
                loop
  loop









fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)



type IVar a = MVar a

newIVar :: IO (IVar a)
newIVar = newEmptyMVar

readIVar :: IVar a -> IO a
readIVar var = readMVar var

putIVar :: IVar a -> a -> IO ()
putIVar var x = do r <- tryPutMVar var x
                   if r then return ()
                        else fail "double put"
