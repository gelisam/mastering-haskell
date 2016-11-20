module Main where

data Statement = PutStrLn String deriving Show
type Logging = [Statement]

hello1 :: Logging
hello1 = replicate 3 (PutStrLn "hello")

hello2 :: Logging
hello2 =
  [ PutStrLn "hello"
  , PutStrLn "hello"
  , PutStrLn "hello"
  ]



































































































main :: IO ()
main = print hello1
