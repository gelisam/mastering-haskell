{-# LANGUAGE Strict #-}
module Main where
import Debug.Trace

main :: IO ()
main = trace "outer" $ trace "inner" $ return ()
