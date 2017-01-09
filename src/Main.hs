{-# LANGUAGE FlexibleInstances, GADTs #-}
module Main where
import Data.MultiSet as MultiSet

data CmdE r where
  IncrementE :: CmdE ()
  ReadE      :: CmdE Int

class LVish c where
  isAllowed :: c r -> MultiSet (SomeR c) -> Bool
  result    :: c r -> MultiSet (SomeR c) -> Maybe r

data SomeR c where
  SomeR :: c r -> SomeR c


instance LVish CmdE where
  isAllowed IncrementE = const True
  isAllowed ReadE      = const True
  result IncrementE = const $ return ()
  result ReadE      = return . MultiSet.occur (SomeR IncrementE)













instance Eq (SomeR CmdE) where
  SomeR IncrementE == SomeR IncrementE = True
  SomeR ReadE      == SomeR ReadE      = True
  _                == _                = False

instance Ord (SomeR CmdE) where
  SomeR IncrementE <= SomeR IncrementE = True
  SomeR IncrementE <= SomeR ReadE      = True
  SomeR ReadE      <= SomeR ReadE      = True
  _                <= _                = False



main :: IO ()
main = return ()
