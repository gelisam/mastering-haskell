module Main where

import Codec.Picture
import Debug.Trace



spriteSize :: FilePath -> IO (Int, Int)
spriteSize filePath = trace "spriteSize" $ do
    Right (ImageRGBA8 image) <- readPng filePath
    return (imageWidth image, imageHeight image)


main :: IO ()
main = do
  putStrLn "example:"
  playerSize <- spriteSize "player.png"
  
  print playerSize
  putStrLn "--"
  print playerSize
