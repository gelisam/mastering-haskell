
module Main where
import Debug.Trace

foo :: [Int] -> Int
foo = go 0
  where
    go acc []     = acc
    go acc (x:xs) = do
      let acc' = trace (show acc ++ " + " ++ show x) $ acc + x
      trace ("go " ++ show acc' ++ " " ++ show xs) $ go acc' xs

main :: IO ()
main = print (foo [1..10] :: Int)
