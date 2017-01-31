module Main where
import Data.IORef
import Data.Time
import qualified Data.MultiMap as MM


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














data User = User deriving (Eq, Ord)



main :: IO ()
main = return ()
