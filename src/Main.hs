module Main where

import Data.Void
import Data.IORef

pull :: IO u
     -> Transform u v Void
     -> IO (v, Transform u v Void)
pull _         (Return bottom) = absurd bottom
pull consumeIO (More t)        = case t of
    Consume cc   -> do u <- consumeIO
                       pull consumeIO (cc u)
    Produce v cc -> return (v, cc ())
    Effect mcc   -> do cc <- mcc
                       pull consumeIO cc

toPull :: Transform u v Void -> IO (PTransform u v)
toPull t0 = do
  ref <- newIORef t0
  return $ PTransform $ \pullU -> do
    t <- readIORef ref
    (v, t') <- pull pullU t
    writeIORef ref t'
    return v












newtype PTransform u v = PTransform
  { onPull :: IO u -> IO v
  }



data TransformF u v a = Consume   (u  -> a)
                      | Produce v (() -> a)
                      | Effect (IO a)

data Transform u v a = Return a
                     | More (TransformF u v (Transform u v a))


type FList a = [a]
data IList a = Cons a (IList a)




instance Functor (TransformF u v) where
  fmap f (Consume   cc) = Consume   (fmap f cc)
  fmap f (Produce v cc) = Produce v (fmap f cc)
  fmap f (Effect   mcc) = Effect    (fmap f mcc)



consume :: Transform u v u
consume = More (Consume Return)

produce :: v -> Transform u v ()
produce v = More (Produce v Return)

effect :: IO a -> Transform u v a
effect mx = More (Effect (fmap Return mx))


instance Functor (Transform u v) where
  fmap f (Return x) = Return (f x)
  fmap f (More cc)  = More (fmap (fmap f) cc)

instance Applicative (Transform u v) where
  pure = Return
  cf <*> cx = do
    f <- cf
    x <- cx
    return (f x)

instance Monad (Transform u v) where
  return = Return
  Return x >>= f = f x
  More cc  >>= f = More (fmap (>>= f) cc)


main :: IO ()
main = return ()
