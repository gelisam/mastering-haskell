module Main where

source :: [Int]
source = [0..]






















sink :: Show a => [a] -> IO ()
sink xs = do
  putStrLn ""
  mapM_ print (take 10 xs)
  putStrLn "..."

main :: IO ()
main = sink source
