{-# LANGUAGE GADTs #-}
module Main where
import Data.Set as Set

data Req
  = VisitRequest Int
  | PopupRequest Int
  deriving (Eq, Ord, Show)


requests :: Signal a -> [Req]
requests = Set.toList . go 0
  where
    go :: Int -> Signal a -> Set Req
    go _     (Pure _)   = Set.empty
    go delay (Ap cc sx) = Set.union (go delay cc) (goF delay sx)
    
    goF :: Int -> SignalF a -> Set Req
    goF delay VisitCount         = Set.singleton (VisitRequest delay)
    goF delay PopupCount         = Set.singleton (PopupRequest delay)
    goF delay (TimeDelayed d sx) = go (delay + d) sx






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
main = print $ requests veryActiveWeek
