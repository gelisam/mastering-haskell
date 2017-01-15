module Main where
import Control.Concurrent.Async

data AsyncList a = Cons a (Async (AsyncList a))
data MsgQueue  a = Cons' a (IVar (MsgQueue a))


newtype AVar a = AVar { getAVar :: Either a (Async a)
                      }

instance Functor AVar where
  fmap f (AVar (Left x))       = AVar (Left (f x))
  fmap f (AVar (Right asyncX)) = AVar (Right (fmap f asyncX))

pureA :: a -> AVar a
pureA = AVar . Left

asyncA :: IO a -> IO (AVar a)
asyncA = fmap (AVar . Right) . async

waitA :: AVar a -> IO a
waitA (AVar (Left x))       = return x
waitA (AVar (Right asyncX)) = wait asyncX



















data IVar a



main :: IO ()
main = return ()
