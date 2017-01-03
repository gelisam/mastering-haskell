module Main where
import Prelude hiding (id, (.))
import Control.Category
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

newtype Pipeline a b = Pipeline { runPipeline :: IVar (MsgQueue a)
                                              -> IVar (MsgQueue b)
                                              -> IO ()
                                }

instance Category Pipeline where
  id = Pipeline $ \qx qx' -> flip evalStateT qx'
                           $ flip evalStateT qx
                           $ forever
                           $ nextMessage >>= lift . sendMessage
  pyz . pxy = Pipeline $ \qx qz -> do
    qy <- newIVar
    void $ forkIO $ runPipeline pxy qx qy
    void $ forkIO $ runPipeline pyz qy qz













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
