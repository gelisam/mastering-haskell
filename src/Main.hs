{-# LANGUAGE BangPatterns #-}
module Main where
import System.IO

main :: IO ()
main = do
  writeFile "foo.txt" $ unlines $ map show [1..10000::Int]
  
  h <- openFile "foo.txt" ReadMode
  let go :: Int -> Int -> IO Int
      go 100 !acc = return acc
      go n   !acc = do
        r <- hIsEOF h
        if r then return acc
             else do x <- read <$> hGetLine h
                     go (n+1) (if odd x then x+acc else acc)
  r <- go 0 0
  print r
