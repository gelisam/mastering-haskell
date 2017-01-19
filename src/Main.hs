module Main where
import Control.Concurrent.Async
import Control.Concurrent.STM

appendTVar :: TVar [a] -> a -> STM ()
appendTVar var x = modifyTVar var (++ [x])











main :: IO ()
main = printUniqueResults [] $ do
  var <- atomically $ newTVar []
  tA <- async $ atomically $ do appendTVar var "A"
                                appendTVar var "AA"
  tB <- async $ atomically $ do appendTVar var "B"
                                appendTVar var "BB"
  mapM_ wait [tA,tB]
  atomically $ readTVar var



printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body
