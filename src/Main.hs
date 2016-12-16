module Main where

data SigTrans a b  -- Signal a -> Signal b

--                    |
--       (t0,click),      (t1,click),       ...
-- False,           True,            False, ...
toggle1 :: SigTrans () Bool
toggle1 = undefined

switchSS :: SigTrans a (SigTrans a b) -> SigTrans a b
switchSS = undefined















data Signal a


main :: IO ()
main = return ()
