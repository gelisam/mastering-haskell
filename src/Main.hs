{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where
import Data.Map as Map
import Data.Semigroup
import Data.Set as Set

newtype CMap k v = CMap (Map k v)

instance (Ord k, Semigroup v) => Semigroup (CMap k v) where
  CMap m <> CMap m' = CMap (unionWith (<>) m m')

instance (Ord k, CRDT v a) => CRDT (CMap k v) (Map k a) where
  value (CMap m) = fmap value m





main :: IO ()
main = do
  let x1 = CMap $ Map.fromList [("A", tt), ("C", ff)]
      x2 = CMap $ Map.fromList [("B", ff), ("C", tt)]
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
