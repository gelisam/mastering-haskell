module Main where
import Data.Function
import Debug.Trace

source :: [Int]
source = map (\x -> trace ("source " ++ show x) x) [1..3]

odds :: [Int] -> [Int]
odds = filter (\x -> trace ("filter " ++ show x) $ odd x)

sink :: [Int] -> Int
sink []     = 0
sink (x:xs) = trace ("sink " ++ show x) (x + sink xs)

main :: IO ()
main = do
  let r = source & odds & sink
  print r
