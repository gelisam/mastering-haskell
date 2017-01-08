module Main where
import Data.MultiSet as MultiSet

data CmdI a = ReadI      | WriteI a  deriving (Eq, Ord)
data CmdF   = IncrementF | FreezeF   deriving (Eq, Ord)
data CmdE   = IncrementE | ReadEvenE deriving (Eq, Ord)

class LVish c where
  isAllowed :: c -> MultiSet c -> Bool

instance Ord a => LVish (CmdI a) where
  isAllowed ReadI      = const True
  isAllowed (WriteI x) = MultiSet.null . MultiSet.filter go
    where go (WriteI x') = x' /= x
          go _           = False

instance LVish CmdF where
  isAllowed FreezeF    = const True
  isAllowed IncrementF = MultiSet.notMember FreezeF

instance LVish CmdE where
  isAllowed IncrementE = const True
  isAllowed ReadEvenE  = even . MultiSet.occur IncrementE


















main :: IO ()
main = return ()
