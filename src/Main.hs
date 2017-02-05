{-# LANGUAGE ExistentialQuantification #-}
module Main where
import Data.List (partition, sort)

data SortingContainer a = forall s. SortingContainer
  { empty  :: s
  , insert :: s -> a -> s  --  <-- API change
  , sorted :: s -> [a]
  }

progressiveImpl :: Ord a => SortingContainer a
progressiveImpl = SortingContainer
  { empty = []
  , insert = \x xs -> let (xsLT, xsGEQ) = partition (< x) xs  -- <-.
                       in xsLT ++ [x] ++ xsGEQ  -- type error -----'
  , sorted = id
  }

justInTimeImpl :: Ord a => SortingContainer a
justInTimeImpl = SortingContainer
  { empty = []
  , insert = (:)  --  <-- type error
  , sorted = sort
  }



main :: IO ()
main = return ()
