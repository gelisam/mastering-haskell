{-# LANGUAGE GADTs #-}
module Main where
import Control.Concurrent

data VState a = VState { value     :: a
                       , isChanged :: Signal
                       }
type TVar a = MVar (VState a)

waitForChange :: Log -> IO ()
waitForChange lg = do someVarChanged <- newSignal
                      go someVarChanged lg
                      block someVarChanged
  where
    go _ Nil                       = return ()
    go s (SnocRead  ops vstate _)  = do _ <- forkIO $ do
                                          block (isChanged vstate)
                                          signal s
                                        go s ops
    go s (SnocWrite ops _      _)  = go s ops






data Log where
  Nil       :: Log
  SnocRead  :: Log -> VState a -> TVar a -> Log
  SnocWrite :: Log -> VState a -> TVar a -> Log

loggedRead :: TVar a -> IO (Log, a)
loggedRead var = do vstate <- readMVar var
                    return (SnocRead Nil vstate var, value vstate)

loggedWrite :: TVar a -> a -> IO (Log, ())
loggedWrite var x' = do vstate <- takeMVar var
                        putMVar var $ vstate { value = x' }
                        return (SnocWrite Nil vstate var, ())

revert :: Log -> IO ()
revert Nil                      = return ()
revert (SnocRead  ops _      _) = revert ops
revert (SnocWrite ops vstate v) = do modifyMVar_ v $ \_ ->
                                       return vstate
                                     revert ops

instance Monoid Log where
  mempty = Nil
  mappend ops = go
    where
      go Nil                  = ops
      go (SnocRead  ops' x v) = SnocRead  (go ops') x v
      go (SnocWrite ops' x v) = SnocWrite (go ops') x v



type Signal = MVar ()

newSignal :: IO Signal
newSignal = newEmptyMVar

reset :: Signal -> IO ()
reset var = do _ <- tryTakeMVar var
               return ()

block :: Signal -> IO ()
block var = do takeMVar var
               signal var

signal :: Signal -> IO ()
signal var = do _ <- tryPutMVar var ()
                return ()



main :: IO ()
main = return ()
