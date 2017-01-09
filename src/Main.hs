module Main where
import Data.MultiSet as MultiSet


data CmdE
  = IncrementE
  | ReadE

class LVish c where
  isAllowed :: c -> MultiSet c -> Bool
  result    :: c -> MultiSet c -> Maybe Result


















data Result

main :: IO ()
main = return ()
