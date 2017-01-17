{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Concurrent
import Control.Concurrent.Async
-- undefined           :: [a]
-- undefined:undefined :: [a]
data AList' a = Nil
              | Cons (AVar a) (AList a)
type AList  a = AVar (AList' a)

push :: AVar a -> MVar (AList a) -> IO ()
push ax var = do axs <- takeMVar var
                 let axs' = pureA $ Cons ax axs
                 putMVar var axs'

waitPop :: MVar (AList a) -> IO (Maybe a)
waitPop var = do axs <- takeMVar var
                 waitA axs >>= \case
                   Nil          -> do putMVar var (pureA Nil)
                                      return Nothing
                   Cons ax axs' -> do x <- waitA ax
                                      putMVar var axs'
                                      return (Just x)











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



-- pretend this is slow
slowAdd :: Int -> Int -> IO Int
slowAdd x1 x2 = return (x1 + x2)



main :: IO ()
main = return ()
