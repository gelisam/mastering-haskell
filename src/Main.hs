{-# LANGUAGE GADTs, MultiParamTypeClasses, OverloadedStrings, TypeFamilies #-}
module Main where
import Data.Hashable
import Haxl.Core

haxlComputation :: GenHaxl () Bool
haxlComputation = do
  r <- or <$> sequence [ dataFetch (PopupRequest 1)
                       , dataFetch (PopupRequest 2)
                       , dataFetch (PopupRequest 3)
                       ]
  if r then and <$> sequence [ dataFetch (PopupRequest 10)
                             , dataFetch (PopupRequest 20)
                             , dataFetch (PopupRequest 30)
                             ]
       else and <$> sequence [ dataFetch (PopupRequest 100)
                             , dataFetch (PopupRequest 200)
                             , dataFetch (PopupRequest 300)
                             ]








delayedVisitCount :: Int -> IO Int
delayedVisitCount days = do
  putStrLn $ "delayedVisitCount " ++ show days
  return 0

hasDisplayedPopup :: Int -> IO Bool
hasDisplayedPopup days = do
  putStrLn $ "hasDisplayedPopup " ++ show days
  return False

filterPopupDays   :: [Int] -> IO [Int]
filterPopupDays days = do
  putStrLn $ "filterPopupDays " ++ show days
  return []



data Req a where
  VisitRequest :: Int -> Req Int
  PopupRequest :: Int -> Req Bool


instance DataSource () Req where
  fetch _ _ _ reqs = SyncFetch $ do
      popupDays <- filterPopupDays (concatMap popupDay reqs)
      forM_ reqs $ \(BlockedFetch req var) -> do
        case req of
          VisitRequest days -> do r <- delayedVisitCount days
                                  putSuccess var r
          PopupRequest days -> do let r = days `elem` popupDays
                                  putSuccess var r
    where
      popupDay :: BlockedFetch Req -> [Int]
      popupDay (BlockedFetch (PopupRequest day) _) = [day]
      popupDay _                                   = []

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

instance Eq (Req a) where
  VisitRequest n == VisitRequest m = n == m
  PopupRequest n == PopupRequest m = n == m

instance Hashable (Req a) where
  hashWithSalt salt (VisitRequest n) = hashWithSalt salt (False, n)
  hashWithSalt salt (PopupRequest n) = hashWithSalt salt (True, n)


runRequest :: Req a -> IO a
runRequest (VisitRequest days) = delayedVisitCount days
runRequest (PopupRequest days) = hasDisplayedPopup days



forM_ :: [a] -> (a -> IO ()) -> IO ()
forM_ = flip mapM_



initialState :: StateStore
initialState = stateSet NoState stateEmpty

main :: IO ()
main = do
  myEnv <- initEnv initialState ()
  _ <- runHaxl myEnv haxlComputation
  return ()
