{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where
import Data.Map as Map
import Data.Semigroup
import Data.Set as Set
import Data.Unique

main :: IO ()
main = do
  x0 <-             add    "A"    ; print (value x0 :: Set String)
  x1 <- (x0 <>) <$> remove "A" x0 ; print (value x1 :: Set String)
  x2 <- (x1 <>) <$> add    "A"    ; print (value x2 :: Set String)
  
  xL1 <- (x2  <>) <$> add    "B"
  
  xR1 <- (x2  <>) <$> add    "B"
  xR2 <- (xR1 <>) <$> remove "B" xR1
  
  print (value (xL1 <> xR2) :: Set String)













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
