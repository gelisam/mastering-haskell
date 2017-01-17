module Main where
import Control.Concurrent.Async
import Control.Monad

-- undefined           :: [a]
-- undefined:undefined :: [a]
data AList' a = Nil
              | Cons (AVar a) (AList a)
type AList  a = AVar (AList' a)

fromList :: [AVar a] -> AList a
fromList []      = pureA Nil
fromList (ax:xs) = pureA $ Cons ax (fromList xs)

waitSpine :: AList a -> IO [AVar a]
waitSpine = waitA >=> go
  where
    go :: AList' a -> IO [AVar a]
    go Nil           = return []
    go (Cons ax axs) = (ax:) <$> waitSpine axs

waitLength :: AList a -> IO Int
waitLength = fmap length . waitSpine






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


main :: IO ()
main = return ()
