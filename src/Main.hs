{-# LANGUAGE GADTs #-}
module Main where

nDaysCount :: Int -> Signal Int
nDaysCount days = (-) <$> visitCount <*> timeDelayed days visitCount

veryActiveWeek :: Signal Bool
veryActiveWeek = (>) <$> nDaysCount 7
                     <*> ((-) <$> nDaysCount 365 <*> nDaysCount 7)

runSignal :: (Int -> IO Int) -> (Int -> IO Int) -> Signal a -> IO a
runSignal delayedVisitCount delayedPopupCount = go 0
  where
    go :: Int -> Signal a -> IO a
    go _     (Pure x)   = return x
    go delay (Ap cc sx) = go delay cc <*> goF delay sx
    
    goF :: Int -> SignalF a -> IO a
    goF delay VisitCount         = delayedVisitCount delay
    goF delay PopupCount         = delayedPopupCount delay
    goF delay (TimeDelayed d sx) = go (delay + d) sx






verboseDelayedVisitCount :: Int -> IO Int
verboseDelayedVisitCount x = do
  putStrLn $ "timeDelayed " ++ show x ++ " visitCount"
  return 0

verboseDelayedPopupCount :: Int -> IO Int
verboseDelayedPopupCount x = do
  putStrLn $ "timeDelayed " ++ show x ++ " popupCount"
  return 0



-- ((f <$> fx) <*> fy) <*> fz
data FreeAp f a where
  Pure :: a -> FreeAp f a
  Ap   :: FreeAp f (e -> a) -> f e -> FreeAp f a

type Signal a = FreeAp SignalF a
data SignalF a where
  VisitCount  :: SignalF Int
  PopupCount  :: SignalF Int
  TimeDelayed :: Int -> Signal a -> SignalF a

visitCount :: Signal Int
visitCount = Ap (Pure id) VisitCount

popupCount :: Signal Int
popupCount = Ap (Pure id) PopupCount

timeDelayed :: Int -> Signal a -> Signal a
timeDelayed days bx = Ap (Pure id) (TimeDelayed days bx)


instance Functor (FreeAp f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Ap fs fe) = Ap (fmap (fmap f) fs) fe

instance Applicative (FreeAp f) where
  pure = Pure
  Pure f   <*> fx = fmap f fx
  Ap fs fe <*> fx = Ap (flip <$> fs <*> fx) fe



main :: IO ()
main = do
  _ <- runSignal verboseDelayedVisitCount
                 verboseDelayedPopupCount
                 veryActiveWeek
  return ()
