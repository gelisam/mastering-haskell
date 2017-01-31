module Main where
import Data.IORef
import Data.Time
import qualified Data.MultiMap as MM
import qualified Data.Map as M

shouldDisplayPopup :: VisitTracker -> PopupTracker -> User -> IO Bool
shouldDisplayPopup visitTracker popupTracker u = do
  t@(UTCTime today time) <- getCurrentTime
  let _48h_ago   = UTCTime (addDays (-2) today) time
  let _7days_ago = UTCTime (addDays (-7) today) time
  
  recordVisit visitTracker u t
  r <- getLastPopup popupTracker u
  if maybe True (<= _48h_ago) r 
  then do totalVisits  <- countVisits      visitTracker u
          recentVisits <- countVisitsSince visitTracker u _7days_ago
          let shouldPopup = totalVisits >= 10
                         || recentVisits >= 3
          when shouldPopup $ do
            recordPopup popupTracker u t
          return shouldPopup
  else return False

















type VisitTracker = IORef (MM.MultiMap User UTCTime)

newVisitTracker :: IO VisitTracker
newVisitTracker = newIORef MM.empty

recordVisit :: VisitTracker -> User -> UTCTime -> IO ()
recordVisit ref u t = do
  modifyIORef ref $ MM.insert u t

countVisits :: VisitTracker -> User -> IO Int
countVisits ref u = do
  length . (MM.! u) <$> readIORef ref

countVisitsSince :: VisitTracker -> User -> UTCTime -> IO Int
countVisitsSince ref u t = do
  length . filter (>= t) . (MM.! u) <$> readIORef ref


type PopupTracker = IORef (M.Map User UTCTime)

newPopupTracker :: IO PopupTracker
newPopupTracker = newIORef M.empty

recordPopup :: PopupTracker -> User -> UTCTime -> IO ()
recordPopup ref u t = do
  modifyIORef ref $ M.insert u t

getLastPopup :: PopupTracker -> User -> IO (Maybe UTCTime)
getLastPopup ref u = do
  M.lookup u <$> readIORef ref



data User = User deriving (Eq, Ord)



when :: Bool -> IO () -> IO ()
when False _    = return ()
when True  body = body



main :: IO ()
main = return ()
