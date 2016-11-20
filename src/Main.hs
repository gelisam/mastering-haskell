module Main where

import Control.Monad


hello1 :: IO ()
hello1 = replicateM_ 3 (putStrLn "hello")

hello2 :: IO ()
hello2 = do
    putStrLn "hello"
    putStrLn "hello"
    putStrLn "hello"



































































































main :: IO ()
main = hello1
