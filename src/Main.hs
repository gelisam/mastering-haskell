module Main where
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  putStrLn "2 + 2 = ?"; void $ forkIO $ slowAdd 2 2 >>= print
  putStrLn "3 + 3 = ?"; void $ forkIO $ slowAdd 3 3 >>= print
  putStrLn "4 + 4 = ?"; void $ forkIO $ slowAdd 4 4 >>= print
  forever $ sleep 1

slowAdd :: Int -> Int -> IO Int
slowAdd x1 x2 = do
  sleep 0.35
  return $ x1 + x2












-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000
