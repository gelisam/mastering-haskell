module Main where

import System.IO.Unsafe


filenameLength :: FilePath -> Int
filenameLength filePath = trace "filenameLength" $ length filePath

contentsLength :: FilePath -> Int
contentsLength filePath = trace "contentsLength" $ unsafePerformIO $ do
    contents <- readFile filePath
    return (length contents)


trace :: String -> a -> a
trace debugMessage x = unsafePerformIO $ do
  putStrLn debugMessage
  return x


main :: IO ()
main = do
  putStrLn "example:"
  
  writeFile "foo.txt" "abcd"
  print $   filenameLength "foo.txt"
  print $   contentsLength "foo.txt"
  
  putStrLn "--"
  
  writeFile "foo.txt" "hello world"
  print $   filenameLength "foo.txt"
  print $   contentsLength "foo.txt"
