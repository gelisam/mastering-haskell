module Main where
import Control.Monad
import Data.Array.IO
import Data.Vector.Mutable as Vector

type SquareMatrix a = IOArray (Int,Int) a

identityMatrix :: Num a => Int -> IO (SquareMatrix a)
identityMatrix n = do
  arr <- newArray ((1,1), (n,n)) 0
  forM_ [1..n] $ \i -> do
    writeArray arr (i,i) 1
  return arr


squares :: Num a => IO (IOVector a)
squares = do
  vect <- Vector.new 4
  Vector.write vect 0 4
  Vector.write vect 1 9
  Vector.write vect 2 16
  Vector.write vect 3 25
  return vect




main :: IO ()
main = do
  arr <- identityMatrix 4
  printSquareMatrix (arr :: SquareMatrix Int)
  vect <- squares
  printVector (vect :: IOVector Int)








printSquareMatrix :: Show a => SquareMatrix a -> IO ()
printSquareMatrix arr = do
  ((i0,j0), (iZ,jZ)) <- getBounds arr
  forM_ [j0..jZ] $ \j -> do
    forM_ [i0..iZ] $ \i -> do
      x <- readArray arr (i,j)
      putStr (show x)
      putStr " "
    putStrLn ""

printVector :: Show a => IOVector a -> IO ()
printVector vect = go (Vector.length vect) []
  where
    go 0 acc = print acc
    go i acc = do
      x <- Vector.read vect (i-1)
      go (i-1) (x:acc)
