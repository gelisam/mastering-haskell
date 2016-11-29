module Main where

add100 :: Int -> Int
add100 = (+100)

transform1 :: [Int] -> [Int]
transform1 = map add100


transform2 :: [Int] -> [Int]
transform2 = filter even


batchesOf :: Int -> [a] -> [[a]]
batchesOf n xs = take n xs : batchesOf n (drop n xs)

transform3 :: [Int] -> [[Int]]
transform3 = batchesOf 5


composed :: [Int] -> [Int]
composed = map sum . batchesOf 5 . filter even













source :: [Int]
source = [0..]

sink :: Show a => [a] -> IO ()
sink xs = do
  putStrLn ""
  mapM_ print (take 5 xs)
  putStrLn "..."

main :: IO ()
main = do
  sink (transform1 source)
  sink (transform2 source)
  sink (transform3 source)
  sink (composed source)
