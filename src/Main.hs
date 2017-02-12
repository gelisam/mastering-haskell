{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where
import Data.Map as Map
import Data.Semigroup
import Data.Set as Set






instance (Ord k, CRDT v a) => CRDT (Map k v) (Map k a) where
  value = fmap value





main :: IO ()
main = do
  let x1 = Map.fromList [("A", tt), ("C", ff)]
      x2 = Map.fromList [("B", ff), ("C", tt)]
  print (value (x1 <> x2) :: Map String Bool)
  print (value (x2 <> x1) :: Map String Bool)



class Semigroup s => CRDT s a where
  value :: s -> a


instance (CRDT s a, CRDT t b) => CRDT (s,t) (a,b) where
  value (s,t) = (value s, value t)


newtype PermanentFlag = PermanentFlag (Set ())
  deriving Semigroup

instance CRDT PermanentFlag Bool where
  value (PermanentFlag s) = s == Set.singleton ()

ff, tt :: PermanentFlag
ff = PermanentFlag Set.empty
tt = PermanentFlag (Set.singleton ())
