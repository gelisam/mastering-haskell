module Main where
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  putStrLn "2 + 2 = ?"; asyncAdd 2 2 print
  putStrLn "3 + 3 = ?"; asyncAdd 3 3 print
  putStrLn "4 + 4 = ?"; asyncAdd 4 4 print
  forever $ sleep 1

slowAdd :: Int -> Int -> IO Int
slowAdd x1 x2 = do
  sleep 0.35
  return $ x1 + x2

asyncAdd :: Int -> Int -> (Int -> IO ()) -> IO ()
asyncAdd x1 x2 cc = void $ forkIO $ slowAdd x1 x2 >>= cc










-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
