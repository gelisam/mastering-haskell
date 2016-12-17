module Main where
import Control.Monad

data Vec2D = Vec2D Double Double
type Image = Vec2D -> Char

mkCircle :: Double -> Image
mkCircle r (Vec2D x y) = if x*x + y*y < r*r then '*' else '.'

zoom3x :: Image -> Image
zoom3x f (Vec2D x y) = f $ Vec2D (x/3) (y/3)

draw :: Double -> Image -> IO ()
draw range image = do
  forM_ [-range..range] $ \y -> do
    forM_ [-range..range] $ \x -> do
      putChar $ image $ Vec2D x y
      putChar $ image $ Vec2D x y
    putChar '\n'

main :: IO ()
main = draw 14 $ zoom3x $ mkCircle 4.4
