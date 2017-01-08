module Main where
import Control.Concurrent
import Control.Monad

fib :: MVar Integer -> Int -> IO ()
fib var 0 = modifyMVar var $ \x -> return (x+1, ())
fib var 1 = modifyMVar var $ \x -> return (x+1, ())
fib var n = fib var (n-1) >> fib var (n-2)

main :: IO ()
main = forM_ [10,20,30,40] $ \n -> do
  print =<< isFibLarge n

isFibLarge :: Int -> IO Bool
isFibLarge n = do
  var <- newMVar 0
  raceM (True <$ waitUntil (> 100) var)
        (do fib var n
            (> 100) <$> readMVar var)

waitUntil :: (a -> Bool) -> MVar a -> IO ()
waitUntil p var = do x <- readMVar var
                     unless (p x) $ do yield
                                       waitUntil p var





raceM :: IO a -> IO a -> IO a
raceM ioX1 ioX2 = do
  var <- newEmptyMVar
  _ <- forkIO $ do x1 <- ioX1; x1 `seq` putMVar var x1
  _ <- forkIO $ do x2 <- ioX2; x2 `seq` putMVar var x2
  readMVar var
