module Main where

data Event a      -- [(Time, a)]
data Behaviour a  -- Time -> a


neverE :: Event a
neverE = undefined

mergeE :: Event a -> Event a -> Event a
mergeE = undefined


pureB :: a -> Behaviour a
pureB = undefined

applyB :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
applyB = undefined









main :: IO ()
main = putStrLn "Welcome to the course!"
