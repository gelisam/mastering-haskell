module Main where
import Control.Concurrent
import Control.Monad

-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000

main :: IO ()
main = do
  var <- newEmptyMVar
  _ <- forkIO $ do
    replicateM_ 4 $ do
      sleep 0.5
      putStrLn "thread"
    putMVar var (42 :: Int)
  
  sleep 0.25
  
  
  replicateM_ 2 $ do
    sleep 0.5
    putStrLn "main"
  print =<< takeMVar var
