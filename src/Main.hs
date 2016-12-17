module Main where
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.Monoid
import System.IO.Unsafe
import System.Process

slow_odd :: Int -> Bool
slow_odd n = unsafePerformIO $ do
  threadDelay (100 * 1000)
  return (odd n)

main :: IO ()
main = do
  animation <- newAnimation
  
  registerPeriodicCallback animation $ \n -> do
    setLeft animation (even n)
  
  registerPeriodicCallback animation $ \n -> do
    setRight animation (slow_odd n)
  
  
  
  
  
  cs <- readIORef (callbacks animation)
  let loop i = do threadDelay (500 * 1000)
                  forM_ cs $ \c -> forkIO $ c i
                  loop (i+1)
  loop 1







registerPeriodicCallback :: Animation -> (Int -> IO ()) -> IO ()
registerPeriodicCallback animation callback =
  modifyIORef (callbacks animation) (callback:)


data Animation = Animation
  { leftRef   :: IORef Bool
  , rightRef  :: IORef Bool
  , callbacks :: IORef [Int -> IO ()]
  , lock      :: MVar ()
  }

newAnimation :: IO Animation
newAnimation = Animation <$> newIORef False
                         <*> newIORef False
                         <*> newIORef []
                         <*> newMVar ()

setLeft :: Animation -> Bool -> IO ()
setLeft animation b = b `seq` withMVar (lock animation) $ \() -> do
  writeIORef (leftRef animation) b
  redraw animation

setRight :: Animation -> Bool -> IO ()
setRight animation b = b `seq` withMVar (lock animation) $ \() -> do
  writeIORef (rightRef animation) b
  redraw animation

redraw :: Animation -> IO ()
redraw animation = do
  leftDir  <- upDown <$> readIORef (leftRef animation)
  rightDir <- upDown <$> readIORef (rightRef animation)
  let image = mkArrow (Vec2D (-10) 0) leftDir
           <> mkArrow (Vec2D   10  0) rightDir
  _ <- system "clear"
  draw 14 image




upDown :: Bool -> Vec2D
upDown True  = Vec2D 0 (-1)  -- up
upDown False = Vec2D 0   1   -- down







type Event a = [(Double, a)]

scanE :: (b -> a -> b) -> b -> Event a -> Event b
scanE _ _ []     = []
scanE f y ((t,x):xs) = let y' = f y x
                       in (t,y') : scanE f y' xs

holdB :: a -> Event a -> Behaviour a
holdB x e t = last $ x : map snd (takeWhile ((< t) . fst) e)


periodicE :: Event ()
periodicE = [(t,()) | t <- [0,0.5..]]



type Behaviour a = Double -> a

differentiate :: Fractional a => Behaviour a -> Behaviour a
differentiate f t = (f (t - 0.1) - f t) / 0.1


mkArrow :: Vec2D -> Vec2D -> Image
mkArrow pos dir = mkCircle 4 (pos - 0.0 * 7 * dir)
               <> mkCircle 3 (pos - 0.3 * 7 * dir)
               <> mkCircle 2 (pos - 0.6 * 7 * dir)
               <> mkCircle 1 (pos - 0.8 * 7 * dir)

position :: Behaviour Vec2D
position t = Vec2D (8 * sin t) (8 * cos t)







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
