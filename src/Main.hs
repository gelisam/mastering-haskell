module Main where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.IO.Unsafe
import Debug.Trace

traceReadTVar :: TVar (String, TVar a) -> STM (TVar a)
traceReadTVar var = do
  (s, v) <- readTVar var
  trace ("got var" ++ s)
      $ return v


main :: IO ()
main = do
  var1 <- atomically $ newTVar []
  varX <- atomically $ newTVar ("1", var1)
  tA <- async $ atomically $ do v <- traceReadTVar varX
                                traceSleep "A" 0.5
                                traceAppendTVar v "AA"
  tB <- async $ atomically $ do v <- traceReadTVar varX
                                traceSleep "B" 0.5
                                traceAppendTVar v "BB"
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
