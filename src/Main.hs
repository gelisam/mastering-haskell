module Main where
import Control.Concurrent
import Control.Exception.Base

withThread :: IO () -> (ThreadId -> IO a) -> IO a
withThread body = bracket (forkIO body) killThread

main :: IO ()
main = do
    withThread (do
        withThread (do
            sleep 1.0
            putStrLn "thread2")
      $ \_ -> do
        sleep 1.0
        putStrLn "thread1")
  $ \thread1 -> do
    
    sleep 0.5
    killThread thread1
    
    sleep 1.0
    putStrLn "main"










-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
