{-# LANGUAGE BangPatterns, GADTs #-}
module Main where
import Data.IORef
import Data.Map as M
import Data.Time

shouldDisplayPopup :: Signal Bool
                   -> IORef Log -> IORef Log
                   -> User -> IO Bool
shouldDisplayPopup shouldPopup visitLog popupLog u = do
  t <- getCurrentTime
  modifyIORef visitLog (record u t)
  b <- runSignal <$> (delayedCount u t <$> readIORef visitLog)
                 <*> (delayedCount u t <$> readIORef popupLog)
                 <*> pure shouldPopup
  when b $ do
    modifyIORef popupLog (record u t)
  return b




















runSignal :: (Int -> Int) -> (Int -> Int) -> Signal a -> a
runSignal delayedVisitCount delayedPopupCount = go 0
  where
    go :: Int -> Signal a -> a
    go _     (Pure x)   = x
    go delay (Ap cc sx) = (go delay cc) (runSignalF delay sx)
    
    runSignalF :: Int -> SignalF a -> a
    runSignalF delay VisitCount         = delayedVisitCount delay
    runSignalF delay PopupCount         = delayedPopupCount delay
    runSignalF delay (TimeDelayed d sx) = go (delay + d) sx



type Log = Map User [UTCTime]

delayedCount :: User -> UTCTime -> Log -> Int -> Int
delayedCount u (UTCTime today time) lg days =
    length $ takeWhile (<= t0) $ M.findWithDefault [] u lg
  where
    t0 = UTCTime (addDays (-fromIntegral days) today) time

record :: User -> UTCTime -> Log -> Log
record u t = M.insertWith (++) u [t]



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



data User = User deriving (Eq, Ord)



when :: Bool -> IO () -> IO ()
when False _    = return ()
when True  body = body



main :: IO ()
main = return ()
