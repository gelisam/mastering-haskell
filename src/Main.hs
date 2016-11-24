module Main where
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map, (!))


-- precondition: the input list must be non-empty
head1 :: [a] -> a
head1 []    = error "head1 of empty list"
head1 (x:_) = x

head2 :: NonEmpty a -> a
head2 (x :| _) = x


-- adjacency list. every neighbour must be a valid key.
type Graph node = Map node [node]

twoEdgesAway :: Ord node => Graph node -> node -> [node]
twoEdgesAway g n0 = nub $ do
  n1 <- g ! n0  -- throws an exception if n0 is not in g
  n2 <- g ! n1  -- throws an exception if n1 is not in g
  return n2






























































































main :: IO ()
main = return ()
