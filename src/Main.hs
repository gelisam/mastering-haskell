module Main where
import Control.Concurrent
import Control.Concurrent.Async

traceReadMVar :: MVar (String, Lock, MVar a) -> IO (Lock, MVar a)
traceReadMVar var = do
  (s, l, v) <- readMVar var
  putStrLn $ "got var" ++ s
  return (l, v)



main :: IO ()
main = do
  lock1 <- newLock; var1 <- newMVar []
  lockX <- newLock; varX <- newMVar ("1", lock1, var1)
  tA <- async $ withLock lockX $ do (l, v) <- traceReadMVar varX
                                    traceSleep "A" 0.5
                                    withLock l $ do
                                      traceAppendMVar v "AA"
  tB <- async $ withLock lockX $ do (l, v) <- traceReadMVar varX
                                    traceSleep "B" 0.5
                                    withLock l $ do
                                      traceAppendMVar v "BB"
  mapM_ wait [tA,tB]



traceAppendMVar :: MVar [String] -> String -> IO ()
traceAppendMVar var s = do
  putStrLn s
  appendMVar var s

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
