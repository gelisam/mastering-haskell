module Main where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.IO.Unsafe
import Debug.Trace








main :: IO ()
main = do
  var1 <- atomically $ newTVar []
  var2 <- atomically $ newTVar []
  tA <- async $ atomically $ do traceAppendTVar var1 "A"
                                traceSleep "A" 0.5
                                traceAppendTVar var2 "AA"
  tB <- async $ atomically $ do traceAppendTVar var2 "B"
                                traceSleep "B" 0.5
                                traceAppendTVar var1 "BB"
  mapM_ wait [tA,tB]





traceAppendTVar :: TVar [String] -> String -> STM ()
traceAppendTVar var s = trace s $ appendTVar var s

appendTVar :: TVar [a] -> a -> STM ()
appendTVar var x = modifyTVar var (++ [x])


traceSleep :: String -> Double -> STM ()
traceSleep t seconds = unsafePerformIO $ do
  putStrLn $ t ++ " is sleeping for " ++ show seconds ++ " seconds"
  sleep seconds
  return $ return ()

-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
