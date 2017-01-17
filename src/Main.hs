{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Concurrent.Async

-- undefined           :: [a]
-- undefined:undefined :: [a]
data AList' a = Nil
              | Cons (AVar a) (AList a)
type AList  a = AVar (AList' a)

waitFold :: (b -> a -> IO b) -> b -> AList a -> IO b
waitFold process y axs = waitA axs >>= \case
  Nil          -> return y
  Cons ax axs' -> do x <- waitA ax
                     y' <- process y x
                     waitFold process y' axs'

waitSum :: AList Int -> IO Int
waitSum = waitFold slowAdd 0






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
