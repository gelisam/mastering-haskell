module Main where



type Behaviour a = Int -> a
type Event     a = Behaviour [a]

mergeE :: Event a -> Event a -> Event a
mergeE e1 e2 = (++) <$> e1 <*> e2





















main :: IO ()
main = return()
