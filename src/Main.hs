module Main where
import Prelude hiding (map)

map :: (u -> v) -> [u] -> [v]
map _ []     = []
map f (u:us) = f u : map f us

mapE :: (u -> IO v) -> EList u a -> EList v a
mapE _       (ReturnE x)             = ReturnE x
mapE convert (MoreE (EffectE mcc))   = do cc <- effectE mcc
                                          mapE convert cc
mapE convert (MoreE (ProduceE u cc)) = do v <- effectE $ convert u
                                          produceE v
                                          mapE convert (cc ())


addIO :: Int -> Int -> IO Int
addIO x y = do putStrLn $ "add " ++ show x ++ " and " ++ show y
               return $ x + y

transform :: EList Int a -> EList Int a
transform = mapE (addIO 100)






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
main = sink (transform source)
