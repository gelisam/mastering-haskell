module Main where
import Control.Monad (when)
import Data.List.Extra (wordsBy)
import System.Environment (getArgs, getEnv)
import System.Exit (exitSuccess)
import Text.Printf (printf)

main :: IO ()
main = do
  writeFile "foo.txt" "abc"
  s <- readFile "foo.txt"
  print s
  
  printf "four digits: %04d\n" (42 :: Int)
  printf "four decimal places: %.4f\n" (pi :: Double)
  
  args <- getArgs
  when (args == ["--help"]) $ putStrLn "usage: ..."
  
  paths <- wordsBy (== ':') <$> getEnv "PATH"
  print $ last paths
  
  _ <- exitSuccess
  print "never printed"
