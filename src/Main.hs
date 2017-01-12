module Main where
import Control.Concurrent
import Control.Exception.Base

syncThread :: IO () -> IO ThreadId
syncThread body = do var <- newEmptyMVar
                     withThread body (putMVar var)
                     takeMVar var

main :: IO ()
main = do
  thread1 <- syncThread $ do
    _ <- syncThread $ do
      sleep 1.0
      putStrLn "thread2"
    
    sleep 1.0
    putStrLn "thread1"
  
  sleep 0.5
  killThread thread1
  
  sleep 1.0
  putStrLn "main"



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000

withThread :: IO () -> (ThreadId -> IO a) -> IO a
withThread body = bracket (forkIO body) killThread
