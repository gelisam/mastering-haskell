module Main where

data SigTrans a b  -- Signal a -> Signal b




pureS :: a -> Signal a
pureS = undefined

applyS :: Signal (a -> b) -> Signal a -> Signal b
applyS = undefined
















data Signal a


main :: IO ()
main = return ()
