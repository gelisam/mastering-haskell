module Main where
import Data.List




main :: IO ()
main = print $ qsort [4,2,7,8,1,6,3]

qsort :: [Int] -> [Int]
qsort []     = []
qsort [x]    = [x]
qsort (x:xs) = let (xsLT, xsGE) = partition (< x) xs
               in qsort xsLT ++ [x] ++ qsort xsGE
