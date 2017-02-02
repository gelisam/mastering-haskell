module Main where
import Data.IORef
import Data.Time
import qualified Data.Map as M


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



main :: IO ()
main = return ()
