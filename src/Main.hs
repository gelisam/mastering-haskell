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

holdB :: Ord a => a -> Event a -> Behaviour a
holdB x e n = let xs = Set.map (holdB x e)
                               (predecesors n)
                  x' = if Set.null xs then x else undefined
              in last (x':e n)

scanE :: Ord b => (b -> a -> b) -> b -> Event a -> Event b
scanE f y e n = let ys = Set.map (holdB y (scanE f y e))
                                 (predecesors n)
                    y' = if Set.null ys then y else undefined
                in tail (scanl' f y' (e n))




















main :: IO ()
main = return()
