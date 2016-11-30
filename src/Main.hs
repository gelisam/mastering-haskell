module Main where


data TransformF u v a = Effect (IO a)
                      | Produce v (() -> a)
                      | Consume   (u  -> a)
data Transform u v a = Return a
                     | More (TransformF u v (Transform u v a))


effect :: IO a -> Transform u v a
effect mx = More (Effect (fmap Return mx))

produce :: v -> Transform u v ()
produce v = More (Produce v Return)

consume :: Transform u v u
consume = More (Consume Return)








runTransform :: IO u -> (v -> IO ()) -> Transform u v a -> IO a
runTransform consumeIO produceIO = go
  where
    go (Return x)            = return x
    go (More (Effect   mcc)) = do cc <- mcc
                                  go cc
    go (More (Produce v cc)) = do produceIO v
                                  go (cc ())
    go (More (Consume   cc)) = do u <- consumeIO
                                  go (cc u)






instance Functor (TransformF u v) where
  fmap f (Effect   mcc) = Effect    (fmap f mcc)
  fmap f (Produce v cc) = Produce v (fmap f cc)
  fmap f (Consume   cc) = Consume   (fmap f cc)

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
