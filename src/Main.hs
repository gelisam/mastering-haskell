module Main where
import Data.Semigroup
import Data.Set as Set

-- (<>) must be commutative and idempotent (and associative).
class Semigroup s => CRDT s
  -- (<>) :: s -> s -> Either Conflict s

instance Ord a => CRDT (Set a)


main :: IO ()
main = do
  let xs  = Set.fromList ["apple","banana"]
      xsL = Set.insert "pineapple" xs
      xsR = Set.insert "melon"     xs
  print (xsL <> xsR)
