{-# LANGUAGE GADTs #-}
module Main where
import Control.Concurrent

type TVar a = MVar a
data Log where
  Nil       :: Log
  SnocRead  :: Log -> a -> TVar a -> Log
  SnocWrite :: Log -> a -> TVar a -> Log

loggedRead :: TVar a -> IO (Log, a)
loggedRead var = do x <- readMVar var
                    return (SnocRead Nil x var, x)

loggedWrite :: TVar a -> a -> IO (Log, ())
loggedWrite var x' = do x <- takeMVar var
                        putMVar var x'
                        return (SnocWrite Nil x var, ())

revert :: Log -> IO ()
revert Nil                 = return ()
revert (SnocRead  ops _ _) = revert ops
revert (SnocWrite ops x v) = do modifyMVar_ v $ \_ -> return x
                                revert ops

instance Monoid Log where
  mempty = Nil
  mappend ops = go
    where
      go Nil                  = ops
      go (SnocRead  ops' x v) = SnocRead  (go ops') x v
      go (SnocWrite ops' x v) = SnocWrite (go ops') x v



main :: IO ()
main = return ()
