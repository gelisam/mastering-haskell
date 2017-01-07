{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent

parOr :: Bool -> Bool -> IO Bool
parOr b1 b2 = raceL (return $ b1 || b2)
                    (return $ b2 || b1)

raceL :: Eq a => IO a -> IO a -> IO a
raceL ioX1 ioX2 = do
  var <- newLVar
  _ <- forkIO $ do !x1 <- ioX1; putLVar var x1
  _ <- forkIO $ do !x2 <- ioX2; putLVar var x2
  readLVar var

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



data LState a = Empty | Full a
type LVar a = MVar (LState a)

newLVar :: IO (LVar a)
newLVar = newMVar Empty

readLVar :: LVar a -> IO a
readLVar var = readMVar var >>= \r -> case r of
                 Full x -> return x
                 Empty  -> do yield
                              readLVar var

putLVar :: Eq a => LVar a -> a -> IO ()
putLVar var x = takeMVar var >>= \r -> case r of
                  Full x' | x == x' -> putMVar var (Full x)
                  Empty             -> putMVar var (Full x)
                  _                 -> fail "incompatible put"
