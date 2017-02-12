{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where
import Data.Semigroup
import Data.Set as Set

class Semigroup s => CRDT s a where
  value :: s -> a

instance CRDT () () where
  value () = ()

instance (CRDT s a, CRDT t b) => CRDT (s,t) (a,b) where
  value (s,t) = (value s, value t)





main :: IO ()
main = do
  let x1 = (tt, (ff, (ff, ())))
      x2 = (ff, (tt, (ff, ())))
  print (value (x1 <> x2) :: (Bool, (Bool, (Bool, ()))))



newtype PermanentFlag = PermanentFlag (Set ())
  deriving Semigroup

instance CRDT PermanentFlag Bool where
  value (PermanentFlag s) = s == Set.singleton ()

ff, tt :: PermanentFlag
ff = PermanentFlag Set.empty
tt = PermanentFlag (Set.singleton ())
