{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Typeable

data SignalState = Blocked [ThreadId] | Signaled
type MySignal = IORef SignalState

data WakeUp = WakeUp deriving (Show, Typeable)
instance Exception WakeUp

block :: MySignal -> IO ()
block ref = do t <- myThreadId
               handle (\WakeUp -> return ()) $ do
                 blocked <- atomicModifyIORef ref $ \case
                   Blocked ts -> (Blocked (t:ts), True)
                   Signaled   -> (Signaled, False)
                 when blocked $ forever $ sleep 600





newSignal :: IO MySignal
newSignal = newIORef (Blocked [])

reset :: MySignal -> IO ()
reset ref = atomicModifyIORef ref $ \case
  Blocked ts -> (Blocked ts, ())
  Signaled   -> (Blocked [], ())

signal :: MySignal -> IO ()
signal ref = do ts <- atomicModifyIORef ref $ \case
                  Blocked ts -> (Signaled, ts)
                  Signaled   -> (Signaled, [])
                forM_ ts $ \t -> throwTo t WakeUp



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000



main :: IO ()
main = return ()
