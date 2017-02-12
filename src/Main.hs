{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where
import Data.Semigroup
import Data.Set as Set

class Semigroup s => CRDT s a where
  value :: s -> a

newtype PermanentFlag = PermanentFlag (Set ())
  deriving (Eq, Semigroup)

instance CRDT PermanentFlag Bool where
  value = (== tt)

ff, tt :: PermanentFlag
ff = PermanentFlag Set.empty
tt = PermanentFlag (Set.singleton ())

main :: IO ()
main = do
  print (value (ff <> ff) :: Bool)
  print (value (ff <> tt) :: Bool)
