{-# LANGUAGE GADTs, MultiParamTypeClasses, OverloadedStrings, TypeFamilies #-}
module Main where
import Data.Hashable
import Haxl.Core


instance Eq (Req a) where
  VisitRequest n == VisitRequest m = n == m
  PopupRequest n == PopupRequest m = n == m

haxlSignal :: Signal a -> GenHaxl () a
haxlSignal = go 0
  where
    go :: Int -> Signal a -> GenHaxl () a
    go _     (Pure x)   = return x
    go delay (Ap cc sx) = go delay cc <*> goF delay sx
    
    goF :: Int -> SignalF a -> GenHaxl () a
    goF delay VisitCount          = dataFetch (VisitRequest delay)
    goF delay HasDisplayedPopup   = dataFetch (PopupRequest delay)
    goF delay (TimeDelayed d sx)  = go (delay + d) sx

delayedVisitCount :: Int -> IO Int
hasDisplayedPopup :: Int -> IO Bool


delayedVisitCount days = do
  putStrLn $ "delayedVisitCount " ++ show days
  return 0

hasDisplayedPopup days = do
  putStrLn $ "hasDisplayedPopup " ++ show days
  return False



data Req a where
  VisitRequest :: Int -> Req Int
  PopupRequest :: Int -> Req Bool


instance DataSource () Req where
  fetch _ _ _ reqs = SyncFetch $ do
    forM_ reqs $ \(BlockedFetch req var) -> do
      r <- runRequest req
      putSuccess var r

instance DataSourceName Req where
  dataSourceName _ = "Req"

instance StateKey Req where
  data State Req = NoState

instance Show1 Req where
  show1 (VisitRequest n) = "VisitRequest " ++ show n
  show1 (PopupRequest n) = "PopupRequest " ++ show n

instance Show (Req a) where
  show (VisitRequest n) = "VisitRequest " ++ show n
  show (PopupRequest n) = "PopupRequest " ++ show n

instance Hashable (Req a) where
  hashWithSalt salt (VisitRequest n) = hashWithSalt salt (False, n)
  hashWithSalt salt (PopupRequest n) = hashWithSalt salt (True, n)


runRequest :: Req a -> IO a
runRequest (VisitRequest days) = delayedVisitCount days
runRequest (PopupRequest days) = hasDisplayedPopup days



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



forM_ :: [a] -> (a -> IO ()) -> IO ()
forM_ = flip mapM_



initialState :: StateStore
initialState = stateSet NoState stateEmpty

main :: IO ()
main = do
  myEnv <- initEnv initialState ()
  _ <- runHaxl myEnv (haxlSignal veryActiveWeek)
  return ()
