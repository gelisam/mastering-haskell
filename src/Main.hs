{-# LANGUAGE BangPatterns #-}
module Main where

data Counter = Counter { field1 :: !Int
                       , field2 :: !Int
                       }

newCounter :: Counter
newCounter = Counter 0 0

assertInvariant :: Counter -> Counter
assertInvariant c@(Counter m n) | m == n    = c
                                | otherwise = error "violation"

increment :: Counter -> Counter
increment = assertInvariant
          . (\(Counter m n) -> Counter (m+1) (n+1))
          . assertInvariant













main :: IO ()
main = return ()
