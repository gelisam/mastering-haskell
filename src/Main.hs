module Main where
import Prelude hiding (id, (.))
import Control.Category

(|>) :: FTransform u v a -> FTransform v w a -> FTransform u w a
ReturnF x             |> _                     = return x
_                     |> ReturnF x             = return x
MoreF (Consume cc1)   |> t2                    = do u <- consume
                                                    cc1 u |> t2
t1                    |> MoreF (Produce w cc2) = do produce w
                                                    t1 |> cc2 ()
MoreF (Produce v cc1) |> MoreF (Consume   cc2) = cc1 () |> cc2 v

instance Category ITransform where
  id = MoreI $ Consume $ \u ->
       MoreI $ Produce u $ \() ->
       id
  t2 . MoreI (Consume cc1) =
    MoreI $ Consume $ \u -> t2 . (cc1 u)
  MoreI (Produce w cc2) . t1 =
    MoreI $ Produce w $ \() -> cc2 () . t1
  MoreI (Consume cc2) . MoreI (Produce v cc1) =
    cc2 v . cc1 ()






















data TransformF u v a = Consume   (u  -> a)
                      | Produce v (() -> a)

data FTransform u v a = ReturnF a
                      | MoreF (TransformF u v (FTransform u v a))
data ITransform u v   = MoreI (TransformF u v (ITransform u v))


runFTransform :: FTransform u v a -> IList u -> (FList v, a)
runFTransform (ReturnF x)            _           = ([], x)
runFTransform (MoreF (Consume   cc)) (Cons u us) = (vs, x)
  where (vs, x) = runFTransform (cc u) us
runFTransform (MoreF (Produce v cc)) us          = (v:vs, x)
  where (vs, x) = runFTransform (cc ()) us

runITransform :: ITransform u v -> IList u -> IList v
runITransform (MoreI (Consume   cc)) (Cons u us) = vs
  where vs = runITransform (cc u) us
runITransform (MoreI (Produce v cc)) us          = v `Cons` vs
  where vs = runITransform (cc ()) us













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
