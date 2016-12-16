module Main where

data Reactive a




scanE :: (a -> b -> a) -> a -> Event b -> Reactive (Event a)
scanE = undefined

holdB :: a -> Event a -> Reactive (Behaviour a)
holdB = undefined


















data Event a
data Behaviour a

main :: IO ()
main = return ()
