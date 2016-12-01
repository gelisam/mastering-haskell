module Main where


import Data.Void

data TransformF u v a = Consume   (u  -> a)
                      | Produce v (() -> a)

data Transform u v a = Return a
                     | More (TransformF u v (Transform u v a))


(|>) :: Transform u v Void
     -> Transform v w Void
     -> Transform u w Void
Return bottom        |> _                    = absurd bottom
_                    |> Return bottom        = absurd bottom
More (Consume cc1)   |> t2                   = do u <- consume
                                                  cc1 u |> t2
t1                   |> More (Produce w cc2) = do produce w
                                                  t1 |> cc2 ()
More (Produce v cc1) |> More (Consume   cc2) = cc1 () |> cc2 v


















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
