module Main where

import System.Mem.StableName

main :: IO ()
main = do
  let xs1 = [1..1000] :: [Int]
  let xs2 = [1..500] ++ [501..1000] :: [Int]

  name1 <- makeStableName xs1
  name2 <- makeStableName xs2
  print (name1 == name1, name1 == name2)
  print (xs1 == xs1, xs1 == xs2)
