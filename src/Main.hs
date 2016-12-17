module Main where
import Control.Concurrent
import Control.Monad
import System.Process

type Animation = [Image]

pulsatingCircle :: Animation
pulsatingCircle = [mkCircle (13.2 * sin t) | t <- [0,0.1..]]

slow3x :: Animation -> Animation
slow3x = concatMap (\x -> [x,x,x])

animate :: Animation -> IO ()
animate []             = return ()
animate (image:images) = do draw 14 image
                            threadDelay (50*1000)
                            _ <- system "clear"
                            animate images

main :: IO ()
main = animate $ slow3x pulsatingCircle













data Vec2D = Vec2D Double Double
type Image = Vec2D -> Char

mkCircle :: Double -> Image
mkCircle r (Vec2D x y) = if x*x + y*y < r*r then '*' else '.'

draw :: Double -> Image -> IO ()
draw range image = do
  forM_ [-range..range] $ \y -> do
    forM_ [-range..range] $ \x -> do
      putChar $ image $ Vec2D x y
      putChar $ image $ Vec2D x y
    putChar '\n'
