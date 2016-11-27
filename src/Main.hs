module Main where
import Control.Monad
import Data.Array
import qualified Data.Vector as V

type SquareMatrix a = Array (Int,Int) a

identityMatrix :: Num a => Int -> SquareMatrix a
identityMatrix n = listArray ((1,1), (n,n)) (repeat 0)
                // [ ((i,i), 1) | i <- [1..n] ]





squares :: Num a => V.Vector a
squares = V.fromList $ (4:) $ V.toList
        $ V.fromList $ (9:) $ V.toList
        $ V.fromList $ (16:) $ V.toList
        $ V.fromList $ (25:) $ V.toList
        $ V.fromList $ []













main :: IO ()
main = do
  printSquareMatrix (identityMatrix 4 :: SquareMatrix Int)
  printVector (squares :: V.Vector Int)


printSquareMatrix :: Show a => SquareMatrix a -> IO ()
printSquareMatrix arr = do
  let ((i0,j0), (iZ,jZ)) = bounds arr
  forM_ [j0..jZ] $ \j -> do
    forM_ [i0..iZ] $ \i -> do
      let x = arr ! (i,j)
      putStr (show x)
      putStr " "
    putStrLn ""

printVector :: Show a => V.Vector a -> IO ()
printVector = print
