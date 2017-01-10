module Main where
import Control.Concurrent
import Control.Exception.Base
import Control.Monad
import Data.Typeable



main :: IO ()
main = do
  var <- newEmptyMVar
  _ <- flip forkFinally (putMVar var) $ do
    replicateM_ 4 $ do
      sleep 0.5
      putStrLn "thread"
      undefined  -- throws "ErrorCall"
    return (42 :: Int)
  
  sleep 0.25
  
  replicateM_ 2 $ do
    sleep 0.5
    putStrLn "main"
  print . either throw id =<< takeMVar var





-- See Volume 1, Video 1.3
typeOfException :: SomeException -> String
typeOfException (SomeException e) = show (typeOf e)

-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
