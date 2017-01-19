module Main where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import System.IO.Unsafe
import Debug.Trace

traceIncrTVar :: String -> TVar Int -> STM ()
traceIncrTVar v var = do
  modifyTVar var (+1)
  x <- readTVar var
  trace ("increment " ++ v ++ " to " ++ show x)
      $ return ()

main :: IO ()
main = do
  var <- atomically $ newTVar 0
  t <- async $ atomically $ do x <- traceReadTVar "A" var
                               check (x >= 3)
                               trace "A DONE"
                                   $ return ()
  replicateM_ 6 $ do sleep 0.5
                     atomically $ traceIncrTVar "var" var
  wait t





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
