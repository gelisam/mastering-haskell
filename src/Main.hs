module Main where
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  putStrLn "2 + 2 = ?"; syncAdd 2 2 >>= print
  putStrLn "3 + 3 = ?"; syncAdd 3 3 >>= print
  putStrLn "4 + 4 = ?"; syncAdd 4 4 >>= print
  forever $ sleep 1

slowAdd :: Int -> Int -> IO Int
slowAdd x1 x2 = do
  sleep 0.35
  return $ x1 + x2

asyncAdd :: Int -> Int -> (Int -> IO ()) -> IO ()
asyncAdd x1 x2 cc = void $ forkIO $ slowAdd x1 x2 >>= cc

syncAdd :: Int -> Int -> IO Int
syncAdd x1 x2 = do
  var <- newEmptyMVar
  asyncAdd x1 x2 (putMVar var)
  takeMVar var








-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
