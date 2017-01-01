module Main where
import System.IO.Unsafe

x, y :: Int
x = unsafePerformIO $ return y
y = unsafePerformIO $ return x

main :: IO ()
main = print x
