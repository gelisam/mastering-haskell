module Main where
import Control.Arrow
import Control.Monad

evenIO :: Int -> IO Bool
evenIO x = do
  if even x
  then return True
  else do putStrLn $ "skipping " ++ show x
          return False

sumIO :: [Int] -> IO Int
sumIO xs = do
  putStrLn $ "computing the sum of " ++ show xs
  return $ sum xs


composed :: EList Int a -> EList Int a
composed = mapE sumIO . batchesOfE 3 . filterE evenIO








mapE :: (u -> IO v) -> EList u a -> EList v a
mapE _       (ReturnE x)             = ReturnE x
mapE convert (MoreE (EffectE mcc))   = do cc <- effectE mcc
                                          mapE convert cc
mapE convert (MoreE (ProduceE u cc)) = do v <- effectE $ convert u
                                          produceE v
                                          mapE convert (cc ())

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


source :: EList Int ()
source = mapM_ produceE [0..]

sink :: Show u => EList u a -> IO ()
sink elist = do
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

main :: IO ()
main = sink (composed source)
