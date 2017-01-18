module Main where
import Control.Concurrent
import Control.Concurrent.Async

traceAppendMVar :: MVar [String] -> String -> IO ()
traceAppendMVar var s = do
  putStrLn s
  appendMVar var s




main :: IO ()
main = do
  lock1 <- newLock; var1 <- newMVar []
  lock2 <- newLock; var2 <- newMVar []
  tA <- async $ withLock lock1 $ do traceAppendMVar var1 "A"
                                    traceSleep "A" 0.5
                                    traceAppendMVar var1 "AA"
  tB <- async $ withLock lock2 $ do traceAppendMVar var2 "B"
                                    traceSleep "B" 0.5
                                    traceAppendMVar var2 "BB"
  mapM_ wait [tA,tB]



traceSleep :: String -> Double -> IO ()
traceSleep t seconds = do
  putStrLn $ t ++ " is sleeping for " ++ show seconds ++ " seconds"
  sleep seconds



type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

withLock :: Lock -> IO a -> IO a
withLock lock body = withMVar lock $ \_ -> body



printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body

appendMVar :: MVar [a] -> a -> IO ()
appendMVar var x = modifyMVar_ var (return . (++ [x]))



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
