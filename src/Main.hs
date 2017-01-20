{-# LANGUAGE GADTs, LambdaCase #-}
module Main where
import Control.Concurrent

data STM a = STM { runSTM :: IO (Log, Either Abort a) }

data Abort = Check




atomically :: STM a -> IO a
atomically sx = runSTM sx >>= \case
  (lg, Right x) -> do commit lg
                      return x
  (lg, Left e)  -> do revert lg
                      case e of
                        Check -> waitForChange lg
                      atomically sx









check :: Bool -> STM ()
check b = STM $ do
  return (Nil, if b then Right () else Left Check)

newTVar :: a -> STM (TVar a)
newTVar x = STM $ do s <- newSignal
                     var <- newMVar $ VState x s
                     return (Nil, Right var)

readTVar :: TVar a -> STM a
readTVar var = STM $ fmap Right <$> loggedRead var

writeTVar :: TVar a -> a -> STM ()
writeTVar var x = STM $ fmap Right <$> loggedWrite var x



instance Functor STM where
  fmap f = STM . (fmap . fmap . fmap) f . runSTM

instance Applicative STM where
  pure = STM . pure . pure . pure
  sf <*> sx = do
    f <- sf
    x <- sx
    return (f x)

instance Monad STM where
  sx >>= f = STM $ do
    (lgX, eitherX) <- runSTM sx
    case eitherX of
      Left e  -> return (lgX, Left e)
      Right x -> do
        (lgY, eitherY) <- runSTM (f x)
        return (mappend lgX lgY, eitherY)



data VState a = VState { value     :: a
                       , isChanged :: Signal
                       }
type TVar a = MVar (VState a)

commit :: Log -> IO ()
commit Nil                      = return ()
commit (SnocRead  ops _      _) = commit ops
commit (SnocWrite ops vstate v) = do
  s <- newSignal
  modifyMVar_ v $ \vstate' ->
    return $ vstate' { isChanged = s }
  signal (isChanged vstate)
  commit ops

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
