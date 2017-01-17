{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections #-}
module Main where
import Control.Concurrent
import Control.Monad
import Data.IORef

data MyMVar a = MyMVar { payload  :: IORef (Maybe a)
                       , okToPut  :: Signal
                       , okToTake :: Signal
                       }

tryTakeMyMVar :: MyMVar a -> IO (Maybe a)
tryTakeMyMVar (MyMVar {..}) = do
  r <- atomicModifyIORef payload (Nothing,)
  when (isJust r) $ signal okToPut
  return r

takeMyMVar :: MyMVar a -> IO a
takeMyMVar var@(MyMVar {..}) = do
  reset okToTake
  tryTakeMyMVar var >>= \case
    Just x  -> do return x
    Nothing -> do block okToTake
                  takeMyMVar var





newEmptyMyMVar :: IO (MyMVar a)
newEmptyMyMVar = MyMVar <$> newIORef Nothing
                        <*> newSignal
                        <*> newSignal

tryPutMyMVar :: MyMVar a -> a -> IO Bool
tryPutMyMVar (MyMVar {..}) x = do
  r <- atomicModifyIORef payload $ \case
    Nothing -> (Just x, True)
    Just x' -> (Just x', False)
  when r $ signal okToTake
  return r

putMyMVar :: MyMVar a -> a -> IO ()
putMyMVar var@(MyMVar {..}) x = do
  reset okToPut
  tryPutMyMVar var x >>= \case
    True  -> return ()
    False -> do block okToPut
                putMyMVar var x



type Signal = MVar ()

newSignal :: IO Signal
newSignal = newEmptyMVar

reset :: Signal -> IO ()
reset = void . tryTakeMVar

block :: Signal -> IO ()
block = takeMVar

signal :: Signal -> IO ()
signal = void . flip tryPutMVar ()

takeWhen :: MVar a -> (a -> Bool) -> Signal -> IO a
takeWhen var p cond = do x <- takeMVar var
                         let satisfied = p x
                         if satisfied
                         then return x
                         else do reset cond
                                 putMVar var x
                                 block cond
                                 takeWhen var p cond



isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

main :: IO ()
main = return ()
