module Main where

data SigTrans a b  -- Signal a -> Signal b

--    :: (Signal a -> Signal b)
--    -> (Signal b -> Signal c)
--    -> (Signal a -> Signal c)
(>>>) :: SigTrans a b -> SigTrans b c -> SigTrans a c
(>>>) = undefined

--  :: (a -> b) -> Signal a -> Signal b
arr :: (a -> b) -> SigTrans a b
arr = undefined

--    :: (Signal a -> Signal b)
--    -> (Signal (a, c) -> Signal (b, c))
first :: SigTrans a b -> SigTrans (a, c) (b, c)
first = undefined











main :: IO ()
main = return ()
