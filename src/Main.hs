module Main where
import Control.Concurrent
import Control.Monad
import System.Process

type Animation = Double -> Image

pulsatingCircle :: Animation
pulsatingCircle t = mkCircle (13.2 * sin t)

slow3x :: Animation -> Animation
slow3x f t = f (t/3)

animate :: Animation -> IO ()
animate animation = go 0
  where go t = do draw 14 (animation t)
                  threadDelay (50*1000)
                  _ <- system "clear"
                  go (t + 0.1)

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
