module Main where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import System.IO.Unsafe
import Debug.Trace


main :: IO ()
main = do
  var1 <- atomically $ newTVar 0
  var2 <- atomically $ newTVar 0
  var3 <- atomically $ newTVar 0
  t <- async $ atomically $ do x <- traceReadTVar "A" var2
                               check (x >= 3)
                               trace "A DONE"
                                   $ return ()
  replicateM_ 6 $ do sleep 0.3
                     atomically $ traceIncrTVar "var1" var1
                     sleep 0.3
                     atomically $ traceIncrTVar "var2" var2
                     sleep 0.3
                     atomically $ traceIncrTVar "var3" var3
  wait t




traceIncrTVar :: String -> TVar Int -> STM ()
traceIncrTVar v var = do
  modifyTVar var (+1)
  x <- readTVar var
  trace ("increment " ++ v ++ " to " ++ show x)
      $ return ()

traceReadTVar :: Show a => String -> TVar a -> STM a
traceReadTVar t var = do
  x <- readTVar var
  trace (t ++ " reads " ++ show x)
      $ return x

traceSleep :: String -> Double -> STM ()
traceSleep t seconds = unsafePerformIO $ do
  putStrLn $ t ++ " is sleeping for " ++ show seconds ++ " seconds"
  sleep seconds
  return $ return ()

-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
