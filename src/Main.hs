module Main where

data SigTrans a b  -- Signal a -> Signal b




pureS :: b -> SigTrans a b
pureS = undefined

applyS :: SigTrans a (b -> c) -> SigTrans a b -> SigTrans a c
applyS = undefined














main :: IO ()
main = return ()
