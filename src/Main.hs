module Main where
import Control.Concurrent
import Control.Concurrent.Async

printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body

appendMVar :: MVar [a] -> a -> IO ()
appendMVar var x = modifyMVar_ var (return . (++ [x]))

main :: IO ()
main = printUniqueResults [] $ do
  var <- newMVar []
  tA <- async $ do appendMVar var "A"
                   appendMVar var "AA"
  tB <- async $ do appendMVar var "B"
                   appendMVar var "BB"
  mapM_ wait [tA,tB]
  readMVar var
