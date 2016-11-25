module Main where
import System.IO.Unsafe
import Text.Printf


verboseSum :: Int -> Int -> Int
verboseSum x y = unsafePerformIO $ do
  printf "adding %d and %d\n" x y
  return (x + y)

generateVerboseProduct :: Int -> Int -> IO Int
generateVerboseProduct x y = unsafeInterleaveIO $ do
  printf "multiplying %d and %d\n" x y
  return (x * y)

main :: IO ()
main = do
  let x1 = verboseSum 6 7
  let x2 = verboseSum 6 7
  x3 <- generateVerboseProduct 6 7
  x4 <- generateVerboseProduct 6 7
  putStrLn "about to print"
  print [x1, x2, x3, x4]
