{-# LANGUAGE GADTs #-}
module Main where

runSignal :: (Int -> IO Int) -> (Int -> IO Int) -> Signal a -> IO a
runSignal delayedVisitCount delayedPopupCount = go 0
  where
    go :: Int -> Signal a -> IO a
    go _     (Pure x)   = return x
    go delay (Ap cc sx) = go delay cc <*> runSignalF delay sx
    
    runSignalF :: Int -> SignalF a -> IO a
    runSignalF delay VisitCount          = delayedVisitCount delay
    runSignalF delay PopupCount          = delayedPopupCount delay
    runSignalF delay (TimeDelayed d sx)  = go (delay + d) sx
    runSignalF delay (IOSignal delayedX) = delayedX delay






















data User = User deriving (Eq, Ord, Read, Show)


data Handle = Handle



-- ((f <$> fx) <*> fy) <*> fz
data FreeAp f a where
  Pure :: a -> FreeAp f a
  Ap   :: FreeAp f (e -> a) -> f e -> FreeAp f a

type Signal a = FreeAp SignalF a
data SignalF a where
  VisitCount  :: SignalF Int
  PopupCount  :: SignalF Int
  TimeDelayed :: Int -> Signal a -> SignalF a
  IOSignal    :: (Int -> IO a) -> SignalF a

visitCount :: Signal Int
visitCount = Ap (Pure id) VisitCount

popupCount :: Signal Int
popupCount = Ap (Pure id) PopupCount

timeDelayed :: Int -> Signal a -> Signal a
timeDelayed days bx = Ap (Pure id) (TimeDelayed days bx)

ioSignal :: (Int -> IO a) -> Signal a
ioSignal delayedX = Ap (Pure id) (IOSignal delayedX)


instance Functor (FreeAp f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Ap fs fe) = Ap (fmap (fmap f) fs) fe

instance Applicative (FreeAp f) where
  pure = Pure
  Pure f   <*> fx = fmap f fx
  Ap fs fe <*> fx = Ap (flip <$> fs <*> fx) fe



main :: IO ()
main = return ()
