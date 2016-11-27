module Main where
import Debug.Trace
import System.Mem.StableName

main :: IO ()
main = do
  let xs1 = trace "xs1" [1..1000] :: [Int]
  let xs2 = trace "xs2" xs1

  name1 <- makeStableName xs1
  name2 <- makeStableName xs2
  print (name1 == name2)
  print (xs1 == xs1, xs1 == xs2)
  
  name1' <- makeStableName xs1
  name2' <- makeStableName xs2
  print (name1' == name2')
  print (name1 == name1', name2 == name2')
