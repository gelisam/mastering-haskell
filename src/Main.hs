module Main where
import Control.Concurrent
import Control.Monad
import Data.List
import Debug.Trace
import Text.Printf

main :: IO ()
main = do
  forM_ (qsort [4,2,7,8,1,6,3]) $ \x -> do
    print x
    sleep 0.8



















qsort :: [Int] -> [Int]
qsort []     = []
qsort [x]    = [x]
qsort (x:xs) = trace (printf "%s: partition (< %s) %s"
                       (show (x:xs)) (show x) (show xs))
             $ let (xsLT, xsGE) = partition (`noisyLT` x) xs
               in qsort xsLT ++ [x] ++ qsort xsGE

noisyLT :: Int -> Int -> Bool
noisyLT x y = trace (printf "%d < %d" x y) (x < y)



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
