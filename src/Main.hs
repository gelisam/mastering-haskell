module Main where

data TransformF u v a = Consume   (u  -> a)
                      | Produce v (() -> a)

data FTransform u v a = ReturnF a
                      | MoreF (TransformF u v (FTransform u v a))
data ITransform u v   = MoreI (TransformF u v (ITransform u v))



















type FList a = [a]
data IList a = Cons a (IList a)




instance Functor (TransformF u v) where
  fmap f (Consume   cc) = Consume   (fmap f cc)
  fmap f (Produce v cc) = Produce v (fmap f cc)



consume :: FTransform u v u
consume = MoreF (Consume ReturnF)

produce :: v -> FTransform u v ()
produce v = MoreF (Produce v ReturnF)


instance Functor (FTransform u v) where
  fmap f (ReturnF x) = ReturnF (f x)
  fmap f (MoreF cc)  = MoreF (fmap (fmap f) cc)

instance Applicative (FTransform u v) where
  pure = ReturnF
  cf <*> cx = do
    f <- cf
    x <- cx
    return (f x)

instance Monad (FTransform u v) where
  return = ReturnF
  ReturnF x >>= f = f x
  MoreF cc  >>= f = MoreF (fmap (>>= f) cc)


main :: IO ()
main = return ()
