module Main where
import Data.MultiSet as MultiSet


data CmdE
  = IncrementE
  | ReadE

class LVish c where
  isAllowed :: c -> MultiSet c -> Bool




















main :: IO ()
main = return ()
