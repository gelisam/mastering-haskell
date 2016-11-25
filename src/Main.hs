{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Exception
import System.IO.Unsafe

main :: IO ()
main = do
  s <- unsafeInterleaveIO $ throwIO (AssertionFailed "oops")
  
  putStrLn "reversing string"
  let! s' = reverse s
  
  putStrLn "done"
