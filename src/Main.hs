module Main where
import Control.Arrow
import Control.Monad
import Data.Void

newtype IEList u = IEList (EList u Void)

instance Functor IEList where
  fmap f (IEList us) = IEList $ mapE (return . f) us

instance Applicative IEList where
  pure x = IEList $ forever $ produceE x
  IEList fs <*> IEList xs = IEList $ go fs xs
    where
      go (ReturnE bottom) _         = absurd bottom
      go _ (ReturnE bottom)         = absurd bottom
      go (MoreE (EffectE mccf)) ccx = do ccf <- effectE mccf
                                         go ccf ccx
      go ccf (MoreE (EffectE mccx)) = do ccx <- effectE mccx
                                         go ccf ccx
      go (MoreE (ProduceE f ccf))
         (MoreE (ProduceE x ccx))   = do produceE (f x)
                                         go (ccf ()) (ccx ())













mapE :: (u -> IO v) -> EList u a -> EList v a
mapE _       (ReturnE x)             = ReturnE x
mapE convert (MoreE (EffectE mcc))   = do cc <- effectE mcc
                                          mapE convert cc
mapE convert (MoreE (ProduceE u cc)) = do v <- effectE $ convert u
                                          produceE v
                                          mapE convert (cc ())

filterIE :: (u -> IO Bool) -> IEList u -> IEList u
filterIE check (IEList elist) = IEList $ filterE check elist

filterE :: (u -> IO Bool) -> EList u a -> EList u a
filterE _     (ReturnE x)            = ReturnE x
filterE check (MoreE (EffectE mcc))   = do cc <- effectE mcc
                                           filterE check cc
filterE check (MoreE (ProduceE u cc)) = do r <- effectE $ check u
                                           when r $ produceE u
                                           filterE check (cc ())

splitAtE :: Int -> EList u a -> EList v ([u], EList u a)
splitAtE 0 cc                     = return ([], cc)
splitAtE _ (ReturnE x)            = return ([], ReturnE x)
splitAtE n (MoreE (EffectE mcc))   = do cc <- effectE mcc
                                        splitAtE n cc
splitAtE n (MoreE (ProduceE u cc)) = first (u:)
                                 <$> splitAtE (n-1) (cc ())

batchesOfE :: Int -> EList u a -> EList [u] a
batchesOfE n elist = do (us, cc) <- splitAtE n elist
                        produceE us
                        batchesOfE n cc





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


source :: IEList Int
source = IEList $ mapM_ go [0..] >> undefined
  where
    go i = do effectE $ putStrLn $ "generating " ++ show i
              produceE i

sink :: Show u => IEList u -> IO ()
sink (IEList elist) = do
    putStrLn ""
    go 5 elist
  where
    go :: Show u => Int -> EList u a -> IO ()
    go 0 _                      = putStrLn "..."
    go _ (ReturnE _)            = putStrLn "..."
    go n (MoreE (EffectE mcc))   = do cc <- mcc
                                      go n cc
    go n (MoreE (ProduceE u cc)) = do print u
                                      go (n-1) (cc ())

checkIO :: Show a => (a -> Bool) -> a -> IO Bool
checkIO p x = do
  if p x
  then return True
  else do putStrLn $ "skipping " ++ show x
          return False

main :: IO ()
main = sink $ (,) <$> filterIE (checkIO (/= 2)) source
                  <*> filterIE (checkIO (/= 4)) source
