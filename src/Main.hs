{-# LANGUAGE GADTs #-}
module Main where
import Haxl.Core

data Req a where
  VisitRequest :: Int -> Req Int
  PopupRequest :: Int -> Req Bool

--         :: Req   -> IO (Either Int Bool)
runRequest :: Req a -> IO a
runRequest (VisitRequest days) = delayedVisitCount days
runRequest (PopupRequest days) = hasDisplayedPopup days










delayedVisitCount :: Int -> IO Int
hasDisplayedPopup :: Int -> IO Bool


delayedVisitCount days = do
  putStrLn $ "delayedVisitCount " ++ show days
  return 0

hasDisplayedPopup days = do
  putStrLn $ "hasDisplayedPopup " ++ show days
  return False



nDaysCount :: Int -> Signal Int
nDaysCount days = (-) <$> visitCount <*> timeDelayed days visitCount

veryActiveWeek :: Signal Bool
veryActiveWeek = (>) <$> nDaysCount 7
                     <*> ((-) <$> nDaysCount 365 <*> nDaysCount 7)



-- ((f <$> fx) <*> fy) <*> fz
data FreeAp f a where
  Pure :: a -> FreeAp f a
  Ap   :: FreeAp f (e -> a) -> f e -> FreeAp f a

type Signal a = FreeAp SignalF a
data SignalF a where
  VisitCount        :: SignalF Int
  HasDisplayedPopup :: SignalF Bool
  TimeDelayed       :: Int -> Signal a -> SignalF a

visitCount :: Signal Int
visitCount = Ap (Pure id) VisitCount

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
  _ <- newResult ()  -- avoid a warning about unused Haxl.Core import
  return ()
