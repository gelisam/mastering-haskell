module Main where
import Data.Set as Set






merge :: Ord a => Set a -> Set a -> Set a
merge = Set.union

main :: IO ()
main = do
  let xs  = Set.fromList ["apple","banana"]
      xsL = Set.insert "pineapple" xs
      xsR = Set.insert "melon"     xs
  print $ merge xsL xsR
