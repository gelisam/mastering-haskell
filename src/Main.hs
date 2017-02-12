{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where
import Data.Map as Map
import Data.Semigroup
import Data.Set as Set
import Data.Unique

-- OR-Set ("Observed Removed")
newtype OR = OR (Set Unique, Set Unique)
  deriving Semigroup

instance CRDT OR Bool where
  value (OR (us,rus)) = not $ Set.null $ Set.difference us rus

add :: a -> IO (CSet a OR)
add x = do
  u <- newUnique
  let v = OR (Set.singleton u, Set.empty)
  return $ CSet $ CMap $ Map.singleton x v

remove :: Ord a => a -> CSet a OR -> IO (CSet a OR)
remove x (CSet (CMap m)) = do
  let OR (us,_) = Map.findWithDefault (OR mempty) x m
  let v = OR (Set.empty, us)
  return $ CSet $ CMap $ Map.singleton x v



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


newtype CSet k b = CSet (CMap k b)
  deriving Semigroup

instance (Ord k, CRDT b Bool) => CRDT (CSet k b) (Set k) where
  value (CSet (CMap m)) = Set.fromList
                        $ Map.keys
                        $ Map.filter value m



main :: IO ()
main = return ()
