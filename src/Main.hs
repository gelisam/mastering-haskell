module Main where
import Control.Concurrent.Async


-- undefined       :: AEither a b
-- Left  undefined :: AEither a b
-- Right undefined :: AEither a b
type AEither a b = AVar (Either (AVar a) (AVar b))



















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
