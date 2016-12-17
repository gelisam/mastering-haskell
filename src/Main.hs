module Main where

import Data.Set as Set

type Node = Int
type Behaviour a = Node -> a
type Event     a = Node -> [a]

edges :: Set (Node, Node)
edges = undefined

mergeE :: Event a -> Event a -> Event a
mergeE e1 e2 = (++) <$> e1 <*> e2




















main :: IO ()
main = return()
