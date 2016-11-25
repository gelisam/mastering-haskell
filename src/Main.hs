{-# LANGUAGE BangPatterns #-}
module Main where


main :: IO ()
main = do
  writeFile "foo.txt" $ unlines $ map show [1..10000::Int]
  
  s <- readFile "foo.txt"
  print (transform s)

transform :: String -> Int
transform = sum . filter odd . take 100 . map read . lines
