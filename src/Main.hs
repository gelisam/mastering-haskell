module Main where

import Prelude hiding (getLine, putStrLn)

import Console


echo :: Console ()
echo = do
  line <- getLine
  runLogging $ putStrLn line


main :: IO ()
main = do
  runConsole echo
  runConsole echo
  runConsole echo
