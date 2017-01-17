module Main where
import Control.Concurrent
import Control.Monad


type Signal = MVar ()

newSignal :: IO Signal
newSignal = newEmptyMVar

reset :: Signal -> IO ()
reset = void . tryTakeMVar

block :: Signal -> IO ()
block var = takeMVar var >> signal var

signal :: Signal -> IO ()
signal = void . flip tryPutMVar ()


















main :: IO ()
main = return ()
