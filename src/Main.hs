module Main where
import Control.Monad
import Data.Void

runTransform :: Transform u v Void -> EList u a -> EList v a
runTransform (Return bottom) _             = absurd bottom
runTransform _               (ReturnE x)   = return x
runTransform (More xform)    (MoreE elist) = go xform elist
  where
    go (Produce v ccT) ccE = do
      produceE v
      runTransform (ccT ()) (MoreE ccE)
    go ccT             (EffectE mccE) = do
      ccE <- effectE mccE
      runTransform (More ccT) ccE
    go (Consume ccT) (ProduceE u ccE) = do
      runTransform (ccT u) (ccE ())


batchesOf :: Int -> Transform u [u] a
batchesOf n = forever $ replicateM n consume >>= produce

batchesOfE :: Int -> EList u a -> EList [u] a
batchesOfE n = runTransform (batchesOf n)





















data TransformF u v a = Consume   (u  -> a)
                      | Produce v (() -> a)
data Transform u v a = Return a
                     | More (TransformF u v (Transform u v a))


consume :: Transform u v u
consume = More (Consume Return)

produce :: v -> Transform u v ()
produce v = More (Produce v Return)


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
    go (ProduceE u cc) = do handle u
                            runEList (cc ()) handle







instance Functor (EListF u) where
  fmap f (EffectE mcc)   = EffectE (fmap f mcc)
  fmap f (ProduceE u cc) = ProduceE u (fmap f cc)

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


source :: EList Int ()
source = mapM_ produceE [0..]

sink :: Show u => EList u a -> IO ()
sink elist = do
    putStrLn ""
    go 10 elist
  where
    go :: Show u => Int -> EList u a -> IO ()
    go 0 _                       = putStrLn "..."
    go _ (ReturnE _)             = putStrLn "..."
    go n (MoreE (EffectE mcc))   = do cc <- mcc
                                      go n cc
    go n (MoreE (ProduceE u cc)) = do print u
                                      go (n-1) (cc ())

main :: IO ()
main = sink (batchesOfE 3 source)
