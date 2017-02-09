{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Static hiding (initRemoteTable)
import Data.Typeable

greet :: String -> String -> IO ()
greet greeting name = putStrLn $ greeting ++ " " ++ name ++ "!"

remotable ['greet]

cGreet :: String -> Closure (String -> IO ())
cGreet = $(mkClosure 'greet)

eval :: Typeable a => Closure a -> a
eval cX = case unclosure remoteTable cX of
  Left  e -> error e
  Right x -> x

main :: IO ()
main = eval (cGreet "hello") "world"










remoteTable :: RemoteTable
remoteTable = __remoteTable initRemoteTable
