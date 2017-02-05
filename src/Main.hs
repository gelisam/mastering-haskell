{-# LANGUAGE ExistentialQuantification #-}
module Main where
import Data.List (partition, sort)

data SortingContainer a = forall s. SortingContainer
  { empty  :: s
  , insert :: s -> a -> s
  , sorted :: s -> [a]
  }

progressiveImpl :: Ord a => SortingContainer a
progressiveImpl = SortingContainer
  { empty = []
  , insert = \xs x -> let (xsLT, xsGEQ) = partition (< x) xs
                       in xsLT ++ [x] ++ xsGEQ
  , sorted = id
  }

justInTimeImpl :: Ord a => SortingContainer a
justInTimeImpl = SortingContainer
  { empty = []
  , insert = flip (:)
  , sorted = sort
  }



main :: IO ()
main = return ()
