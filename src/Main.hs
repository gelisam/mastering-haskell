module Main where
import Control.Concurrent
import Control.Monad
import Data.Monoid
import System.Process

type Behaviour a = Double -> a

differentiate :: Fractional a => Behaviour a -> Behaviour a
differentiate f t = (f (t - 0.1) - f t) / 0.1


mkArrow :: Vec2D -> Vec2D -> Image
mkArrow pos dir = mkCircle 4 (pos - 0.0 * dir)
               <> mkCircle 3 (pos - 0.3 * dir)
               <> mkCircle 2 (pos - 0.6 * dir)
               <> mkCircle 1 (pos - 0.8 * dir)

position :: Behaviour Vec2D
position t = Vec2D (8 * sin t) (8 * cos t)

main :: IO ()
main = animate $ mkArrow <$> position <*> differentiate position







instance Monoid Char where
  mempty = '.'
  mappend '.' c = c
  mappend c '.' = c
  mappend c _ = c


instance Num a => Num (t -> a) where
  b1 + b2 = (+) <$> b1 <*> b2
  b1 * b2 = (+) <$> b1 <*> b2
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (t -> a) where
  recip = fmap recip
  fromRational = pure . fromRational


type Animation = Double -> Image

animate :: Animation -> IO ()
animate animation = go 0
  where go t = do draw 14 (animation t)
                  threadDelay (50*1000)
                  _ <- system "clear"
                  go (t + 0.1)





data Vec2D = Vec2D Double Double

instance Num Vec2D where
  Vec2D x1 y1 + Vec2D x2 y2 = Vec2D (x1+x2) (y1+y2)
  Vec2D x1 y1 * Vec2D x2 y2 = Vec2D (x1*x2) (y1*y2)
  abs (Vec2D x y) = Vec2D (abs x) (abs y)
  signum (Vec2D x y) = Vec2D (signum x) (signum y)
  negate (Vec2D x y) = Vec2D (negate x) (negate y)
  fromInteger n = Vec2D (fromInteger n) (fromInteger n)

instance Fractional Vec2D where
  recip (Vec2D x y) = Vec2D (recip x) (recip y)
  fromRational n = Vec2D (fromRational n) (fromRational n)



type Image = Vec2D -> Char

translate :: Vec2D -> Image -> Image
translate (Vec2D dx dy) f (Vec2D x y) = f $ Vec2D (x-dx) (y-dy)

mkCircle :: Double -> Vec2D -> Image
mkCircle r dxy = translate dxy
               $ \(Vec2D x y) -> if x*x + y*y < r*r
                                 then '*'
                                 else '.'

draw :: Double -> Image -> IO ()
draw range image = do
  forM_ [-range..range] $ \y -> do
    forM_ [-range..range] $ \x -> do
      putChar $ image $ Vec2D x y
      putChar $ image $ Vec2D x y
    putChar '\n'
