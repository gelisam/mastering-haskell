module Main where
import Data.Function


source :: [Int]
source = [1..100]

odds :: [Int] -> [Int]
odds = filter odd

sink :: [Int] -> Int
sink = sum


main :: IO ()
main = do
  let r = source & odds & sink
  print r
