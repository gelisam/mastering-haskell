{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where
import Data.Map as Map
import Data.Semigroup
import Data.Set as Set

newtype CSet k b = CSet (CMap k b)
  deriving Semigroup

instance (Ord k, CRDT b Bool) => CRDT (CSet k b) (Set k) where
  value (CSet (CMap m)) = Set.fromList
                        $ Map.keys
                        $ Map.filter value m





main :: IO ()
main = do
  let x1 = CSet $ CMap $ Map.fromList [("A", tt)]
      x2 = CSet $ CMap $ Map.fromList [("B", ff)]
  print (value (x1 <> x2) :: Set String)



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


newtype CMap k v = CMap (Map k v)

instance (Ord k, Semigroup v) => Semigroup (CMap k v) where
  CMap m1 <> CMap m2 = CMap (unionWith (<>) m1 m2)

instance (Ord k, CRDT v a) => CRDT (CMap k v) (Map k a) where
  value (CMap m) = fmap value m
