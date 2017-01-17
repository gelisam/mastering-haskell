{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Concurrent
import Control.Monad

data BoundedBuffer a = BoundedBuffer { buffer      :: MVar [a]
                                     , okToProduce :: Signal
                                     , okToConsume :: Signal
                                     }

producer :: BoundedBuffer Int -> IO ()
producer (BoundedBuffer {..}) = go 0
  where
    go x = do sleep 0.3
              full <- modifyMVar buffer $ \xs -> do
                let xs' = xs ++ [x]
                let full = length xs' == 4
                putStrLn $ "PRODUCER " ++ show xs'
                return (xs', full)
              signal okToConsume
              when full $ do                   -- consumer empties
                sleep 2                        -- the buffer here
                block okToProduce
              go (x+1)









newBoundedBuffer :: IO (BoundedBuffer a)
newBoundedBuffer = BoundedBuffer <$> newMVar []
                                 <*> newSignal
                                 <*> newSignal

consumer :: BoundedBuffer Int -> IO ()
consumer (BoundedBuffer {..}) = go
  where
    go = do sleep 0.4
            empty <- modifyMVar buffer $ \xs -> do
              let xs' = drop 1 xs
              let empty = length xs' == 0
              putStrLn $ "CONSUMER " ++ show xs'
              return (xs', empty)
            signal okToProduce
            when empty $ do
              block okToConsume
            go



type Signal = MVar ()

newSignal :: IO Signal
newSignal = newEmptyMVar

block :: Signal -> IO ()
block cond = do _ <- tryTakeMVar cond
                takeMVar cond
                signal cond

signal :: Signal -> IO ()
signal cond = do _ <- tryPutMVar cond ()
                 return ()



main :: IO ()
main = do
  bounderBuffer <- BoundedBuffer <$> newMVar []
                                 <*> newSignal
                                 <*> newSignal
  _ <- forkIO $ producer bounderBuffer
  _ <- forkIO $ consumer bounderBuffer
  
  let loop = do sleep 1
                loop
  loop



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
