module Main where

import Data.Void


runForever :: IO u
           -> (v -> IO ())
           -> Transform u v Void
           -> IO ()
runForever _         _         (Return bottom) = absurd bottom
runForever consumeIO produceIO (More t)        = case t of
    Consume cc   -> do u <- consumeIO
                       runForever consumeIO produceIO (cc u)
    Produce v cc -> do produceIO v
                       runForever consumeIO produceIO (cc ())
    Effect mcc   -> do cc <- mcc
                       runForever consumeIO produceIO cc
















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
