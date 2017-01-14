module Main where
import Data.List
import Debug.Trace
import Text.Printf


main :: IO ()
main = print $ take 4 $ qsort [4,2,7,8,1,6,3]

qsort :: [Int] -> [Int]
qsort []     = []
qsort [x]    = [x]
qsort (x:xs) = trace (printf "%s: partition (< %s) %s"
                       (show (x:xs)) (show x) (show xs))
             $ let (xsLT, xsGE) = partition (`noisyLT` x) xs
               in qsort xsLT ++ [x] ++ qsort xsGE

noisyLT :: Int -> Int -> Bool
noisyLT x y = trace (printf "%d < %d" x y) (x < y)
