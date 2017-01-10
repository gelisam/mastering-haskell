module Main where
import Control.Concurrent





main :: IO ()
main = do
    thread1 <- forkIO $ do
        _ <- forkIO $ do
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
