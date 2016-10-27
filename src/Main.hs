module Main where

import Codec.Picture
import Debug.Trace
import System.IO.Unsafe


spriteSize :: FilePath -> (Int, Int)
spriteSize filePath = trace "spriteSize" $ unsafePerformIO $ do
    Right (ImageRGBA8 image) <- readPng filePath
    return (imageWidth image, imageHeight image)


main :: IO ()
main = do
  putStrLn "example:"
  
  
  print $ spriteSize "player.png"
  putStrLn "--"
  print $ spriteSize "player.png"
