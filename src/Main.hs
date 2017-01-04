module Main where
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

type Step a = IVar (Progress a) -> IO ()
data Progress a = Done a | More (Step a)

yield :: Step a -> Step a
yield cc var = putIVar var (More cc)

done :: a -> Step a
done s var = putIVar var (Done s)

tryReadIVar :: StateT (IVar (Progress a)) IO (Maybe a)
tryReadIVar = do var <- get
                 r <- liftIO $ readIVar var
                 case r of Done x  -> return (Just x)
                           More cc -> do
                             var' <- liftIO newIVar
                             void $ liftIO $ forkIO $ cc var'
                             put var'
                             return Nothing



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
