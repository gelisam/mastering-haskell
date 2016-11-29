module Main where

add100 :: Int -> Int
add100 = (+100)

transform1 :: [Int] -> [Int]
transform1 = map add100




















source :: [Int]
source = [0..]

sink :: Show a => [a] -> IO ()
sink xs = do
  putStrLn ""
  mapM_ print (take 5 xs)
  putStrLn "..."

main :: IO ()
main = sink (transform1 source)
