module Main where
import Control.Concurrent
import Control.Concurrent.Async

type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

withLock :: Lock -> IO a -> IO a
withLock lock body = withMVar lock $ \_ -> body

main :: IO ()
main = printUniqueResults [] $ do
  lock <- newLock
  var <- newMVar []
  tA <- async $ withLock lock $ do appendMVar var "A"
                                   appendMVar var "AA"
  tB <- async $ withLock lock $ do appendMVar var "B"
                                   appendMVar var "BB"
  mapM_ wait [tA,tB]
  readMVar var



printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body

appendMVar :: MVar [a] -> a -> IO ()
appendMVar var x = modifyMVar_ var (return . (++ [x]))
