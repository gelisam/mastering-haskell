{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where
import Data.Map as Map
import Data.Semigroup
import Data.Set as Set

-- 2P-Set ("2 Phases")
newtype AddRemove = AddRemove (PermanentFlag,PermanentFlag)
  deriving Semigroup

instance CRDT AddRemove Bool where
  value (AddRemove x) = case value x of
    (True, False) -> True
    _             -> False

add :: a -> CSet a AddRemove
add x = CSet $ CMap $ Map.singleton x $ AddRemove (tt,ff)

remove :: a -> CSet a AddRemove
remove x = CSet $ CMap $ Map.singleton x $ AddRemove (tt,tt)






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
