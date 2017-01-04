module Main where
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

data MsgQueue s   = Cons s (IVar (MsgQueue s))
type QueueT s m a = StateT (IVar (MsgQueue s)) m a

sendMessage :: MonadIO m => s -> QueueT s m ()
sendMessage s = do var <- get
                   var' <- liftIO newIVar
                   liftIO $ putIVar var $ Cons s var'

nextMessage :: MonadIO m => QueueT s m s
nextMessage = do var <- get
                 Cons x var' <- liftIO $ readIVar var
                 put var'
                 return x

















type IVar a = MVar a

newIVar :: IO (IVar a)
newIVar = newEmptyMVar

readIVar :: IVar a -> IO a
readIVar var = readMVar var

putIVar :: IVar a -> a -> IO ()
putIVar var x = do r <- tryPutMVar var x
                   when (not r) $ fail "double put"
















main :: IO ()
main = return ()
