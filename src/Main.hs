{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Data.Semigroup
import Data.Set as Set

class Semigroup s => CRDT s a where
  value :: s -> a

instance Ord a => CRDT (Set a) (Set a) where
  value = id

main :: IO ()
main = do
  let xs  = Set.fromList ["apple","banana"]
      xsL = Set.insert "pineapple" xs
      xsR = Set.insert "melon"     xs
  print (value (xsL <> xsR) :: Set String)
