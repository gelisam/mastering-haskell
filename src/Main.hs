{-# LANGUAGE GADTs #-}
module Main where

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



lastWeekVisitCount :: Signal Int
lastWeekVisitCount = (-) <$> visitCount <*> timeDelayed 7 visitCount

shownInterest :: Signal Bool
shownInterest = (||) <$> ((>= 10) <$> visitCount)
                     <*> ((>=  3) <$> lastWeekVisitCount)

recentPopup :: Signal Bool
recentPopup = (>) <$> popupCount <*> timeDelayed 2 popupCount

shouldPopup :: Signal Bool
shouldPopup = (&&) <$> shownInterest 
                   <*> (not <$> recentPopup)



main :: IO ()
main = return ()
