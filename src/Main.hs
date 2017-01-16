module Main where
import Control.Concurrent

producer :: MVar [Int] -> IO ()
producer var = go 0
  where
    go x = do sleep 0.3
              modifyMVar_ var $ \xs -> do
                let xs' = xs ++ [x]
                putStrLn $ "PRODUCER " ++ show xs'
                return xs'
              go (x+1)

consumer :: MVar [Int] -> IO ()
consumer var = go
  where
    go = do sleep 0.4
            modifyMVar_ var $ \xs -> do
              let xs' = drop 1 xs
              putStrLn $ "CONSUMER " ++ show xs'
              return xs'
            go








main :: IO ()
main = do
  var <- newMVar []
  _ <- forkIO $ producer var
  _ <- forkIO $ consumer var
  
  let loop = do sleep 1
                loop
  loop



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
