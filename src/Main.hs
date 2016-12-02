module Main where


data EListF u a = EffectE (IO a)
                | ProduceE u (() -> a)

data EList u a = ReturnE a
               | MoreE (EListF u (EList u a))


effectE :: IO a -> EList u a
effectE mx = MoreE (EffectE (fmap ReturnE mx))

produceE :: u -> EList u ()
produceE u = MoreE (ProduceE u ReturnE)











runEList :: EList u a -> (u -> IO ()) -> IO a
runEList (ReturnE x)    _      = return x
runEList (MoreE eListF) handle = go eListF
  where
    go (EffectE   mcc) = do cc <- mcc
                            runEList cc handle
    go (ProduceE v cc) = do handle v
                            runEList (cc ()) handle







instance Functor (EListF u) where
  fmap f (EffectE mcc)   = EffectE (fmap f mcc)
  fmap f (ProduceE v cc) = ProduceE v (fmap f cc)

instance Functor (EList u) where
  fmap f (ReturnE x) = ReturnE (f x)
  fmap f (MoreE cc)  = MoreE (fmap (fmap f) cc)

instance Applicative (EList u) where
  pure = ReturnE
  cf <*> cx = do
    f <- cf
    x <- cx
    return (f x)

instance Monad (EList u) where
  return = ReturnE
  ReturnE x >>= f = f x
  MoreE cc  >>= f = MoreE (fmap (>>= f) cc)


main :: IO ()
main = return ()
