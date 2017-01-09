{-# LANGUAGE GADTs #-}
module Main where
import Data.MultiSet as MultiSet

data CmdE r where
  IncrementE :: CmdE ()
  ReadE      :: CmdE Int

class LVish c where
  isAllowed :: c r -> MultiSet c -> Bool
  result    :: c r -> MultiSet c -> Maybe r



















main :: IO ()
main = return ()
