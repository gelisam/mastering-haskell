module Main where
import Control.Concurrent


runInParallel :: IO a -> IO b -> IO (a,b)
runInParallel ioX ioY = do
  varX <- newEmptyMVar
  varY <- newEmptyMVar
  _ <- forkIO $ putMVar varX =<< ioX
  _ <- forkIO $ putMVar varY =<< ioY
  (,) <$> takeMVar varX <*> takeMVar varY

runAtomically :: MVar () -> IO a -> IO a
runAtomically mutex ioX = do
  takeMVar mutex
  r <- ioX
  putMVar mutex ()
  return r



















main :: IO ()
main = return ()
