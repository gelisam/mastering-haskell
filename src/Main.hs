module Main where
import Control.Concurrent
import Control.Concurrent.STM

type BoundedBuffer a = TVar [a]

producer :: BoundedBuffer Int -> IO ()
producer buffer = go 0
  where
    go x = do sleep 0.3
              xs' <- atomically $ do xs <- readTVar buffer
                                     let xs' = xs ++ [x]
                                     writeTVar buffer xs'
                                     check (length xs' <= 4)
                                     return xs'
              putStrLn $ "PRODUCER " ++ show xs'
              go (x+1)









consumer :: BoundedBuffer Int -> IO ()
consumer buffer = go
  where
    go = do sleep 0.4
            xs' <- atomically $ do xs <- readTVar buffer
                                   check (length xs > 0)
                                   let xs' = tail xs
                                   writeTVar buffer xs'
                                   return xs'
            putStrLn $ "CONSUMER " ++ show xs'
            go



newBoundedBuffer :: IO (BoundedBuffer a)
newBoundedBuffer = atomically $ newTVar []



main :: IO ()
main = do
  bounderBuffer <- newBoundedBuffer
  _ <- forkIO $ producer bounderBuffer
  _ <- forkIO $ consumer bounderBuffer
  
  let loop = do sleep 1
                loop
  loop



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
