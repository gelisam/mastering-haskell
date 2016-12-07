module Main where


data Signal a  --  (0, a)  +  [(Time, a)]  =  Time -> a


--neverS :: Signal a
--neverS = undefined

mergeS :: Signal a -> Signal a -> Signal a
mergeS = undefined


pureS :: a -> Signal a
pureS = undefined

applyS :: Signal (a -> b) -> Signal a -> Signal b
applyS = undefined













main :: IO ()
main = putStrLn "Welcome to the course!"
