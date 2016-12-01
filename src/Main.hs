module Main where
import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Data.Void
data TransformF u v a = Consume   (u  -> a)
                      | Produce v (() -> a)

data Transform u v a = Return a
                     | More (TransformF u v (Transform u v a))
newtype ITransform u v = ITransform (Transform u v Void)

instance Category ITransform where
  id = ITransform $ forever $ consume >>= produce
  ITransform t2 . ITransform t1 = ITransform $ go t1 t2
    where
      go (More (Consume cc1)) cc2   = do u <- consume
                                         go (cc1 u) cc2
      go cc1 (More (Produce w cc2)) = do produce w
                                         go cc1 (cc2 ())
      go (More (Produce v cc1)) (More (Consume cc2))
                                    = go (cc1 ()) (cc2 v)
      go _ _ = error "impossible case returning a Void"
















type FList a = [a]
data IList a = Cons a (IList a)




instance Functor (TransformF u v) where
  fmap f (Consume   cc) = Consume   (fmap f cc)
  fmap f (Produce v cc) = Produce v (fmap f cc)



consume :: Transform u v u
consume = More (Consume Return)

produce :: v -> Transform u v ()
produce v = More (Produce v Return)


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
