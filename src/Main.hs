module Main where

data Reactive a




neverE :: Event a
neverE = undefined

mergeE :: Event a -> Event a -> Event a
mergeE = undefined

pureB :: a -> Behaviour a
pureB = undefined

applyB :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
applyB = undefined

applyE :: Behaviour (a -> b) -> Event a -> Event b
applyE = undefined

mapFilterE :: (a -> Maybe b) -> Event a -> Event b
mapFilterE = undefined


















data Event a
data Behaviour a

main :: IO ()
main = return ()
