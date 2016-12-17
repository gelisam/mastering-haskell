module Main where
import Data.List
import Data.Set as Set

type Node = Int
type Behaviour a = Node -> a
type Event     a = Node -> [a]

edges :: Set (Node, Node)
edges = undefined

predecesors :: Node -> Set Node
predecesors n = Set.map fst $ Set.filter ((== n) . snd) edges

holdB :: Ord a => (Set a -> a) -> Event a -> Behaviour a
holdB g e n = let xs = Set.map (holdB g e)
                               (predecesors n)
                  x' = g xs
              in last (x':e n)

scanE :: Ord b => (b->a->b) -> (Set b -> b) -> Event a -> Event b
scanE f g e n = let ys = Set.map (holdB g (scanE f g e))
                                 (predecesors n)
                    y' = g ys
                in tail (scanl' f y' (e n))




















main :: IO ()
main = return()
