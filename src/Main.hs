module Main where
import Control.Concurrent
import Control.Monad



type Step a = IVar (Progress a) -> IO ()
data Progress a = Done a | More (Step a)

yield :: Step a -> Step a
yield cc var = putIVar var (More cc)

done :: a -> Step a
done s var = putIVar var (Done s)













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
