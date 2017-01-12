module Main where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

main :: IO ()
main = do
  rs <- replicateM 10 $ do
    var <- newMVar (0 :: Int)
    runConcurrently $ const <$> Concurrently (readMVar var)
                            <*> Concurrently (swapMVar var 1)
  print rs
