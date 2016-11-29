module Main where
import Data.Void


data TransformF u v a = Consume   (u  -> a)
                      | Produce v (() -> a)
data Transform u v a = Return a
                     | More (TransformF u v (Transform u v a))


consume :: Transform u v u
consume = More (Consume Return)

produce :: v -> Transform u v ()
produce v = More (Produce v Return)


runTransform :: Transform u v Void -> [u] -> [v]
runTransform (Return _)    = error "early return"
runTransform (More xform)  = go xform
  where
    go (Consume   _ ) []     = error "early end of stream"
    go (Consume   cc) (u:us) = runTransform (cc u) us
    go (Produce v cc) us     = v : runTransform (cc ()) us







instance Functor (TransformF u v) where
  fmap f (Consume   cc) = Consume   (fmap f cc)
  fmap f (Produce v cc) = Produce v (fmap f cc)

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
