{-# LANGUAGE GADTs #-}
module Main where
import Data.Map as Map

data Req
  = VisitRequest Int
  | PopupRequest Int
  deriving (Eq, Ord, Show)
type Response = Either Int Bool

cachedSignal :: Map Req Response -> Signal a -> a
cachedSignal c = go 0
  where
    go :: Int -> Signal a -> a
    go _     (Pure x)   = x
    go delay (Ap cc sx) = go delay cc $ goF delay sx
    
    goF :: Int -> SignalF a -> a
    goF delay VisitCount         = fromLeft  (c ! VisitRequest delay)
    goF delay HasDisplayedPopup  = fromRight (c ! PopupRequest delay)
    goF delay (TimeDelayed d sx) = go (delay + d) sx

runSignal :: Signal a -> IO a
runSignal signal = do c <- cacheRequests (requests signal)
                      return $ cachedSignal c signal



nDaysCount :: Int -> Signal Int
nDaysCount days = (-) <$> visitCount <*> timeDelayed days visitCount

veryActiveWeek :: Signal Bool
veryActiveWeek = (>) <$> nDaysCount 7
                     <*> ((-) <$> nDaysCount 365 <*> nDaysCount 7)



requests :: Signal a -> [Req]
requests = nub . go 0
  where
    go :: Int -> Signal a -> [Req]
    go _     (Pure _)   = []
    go delay (Ap cc sx) = go delay cc ++ goF delay sx
    
    goF :: Int -> SignalF a -> [Req]
    goF delay VisitCount         = [VisitRequest delay]
    goF delay HasDisplayedPopup  = [PopupRequest delay]
    goF delay (TimeDelayed d sx) = go (delay + d) sx



delayedVisitCount :: Int -> IO Int
delayedVisitCount days = do
  putStrLn $ "delayedVisitCount " ++ show days
  return 0

hasDisplayedPopup :: Int -> IO Bool
hasDisplayedPopup days = do
  putStrLn $ "hasDisplayedPopup " ++ show days
  return False

runRequest :: Req -> IO Response
runRequest (VisitRequest days) = Left  <$> delayedVisitCount days
runRequest (PopupRequest days) = Right <$> hasDisplayedPopup days

cacheRequests :: [Req] -> IO (Map Req Response)
cacheRequests rs = Map.fromList <$> mapM go rs
  where
    go :: Req -> IO (Req, Response)
    go r = do x <- runRequest r
              return (r, x)



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



nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub (Prelude.filter (/= x) xs)


fromLeft :: Either a b -> a
fromLeft (Left  x) = x
fromLeft (Right _) = error "fromLeft (Right ...)"

fromRight :: Either a b -> b
fromRight (Left  _) = error "fromRight (Right ...)"
fromRight (Right y) = y



main :: IO ()
main = do
  _ <- runSignal veryActiveWeek
  return ()
