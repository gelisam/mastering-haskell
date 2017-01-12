module Main where
import Control.Concurrent
import Control.Exception.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

fork :: ContT () IO () -> ContT r IO ThreadId
fork = ContT . withThread . evalContT

main :: IO ()
main = evalContT $ do
  thread1 <- fork $ do
    _ <- fork $ do
      lift $ sleep 1.0
      lift $ putStrLn "thread2"
    
    lift $ sleep 1.0
    lift $ putStrLn "thread1"
  
  lift $ sleep 0.5
  lift $ killThread thread1
  
  lift $ sleep 1.0
  lift $ putStrLn "main"



-- like threadDelay, but using seconds instead of microseconds
sleep :: Double -> IO ()
sleep seconds = threadDelay $ round $ seconds * 1000 * 1000

withThread :: IO () -> (ThreadId -> IO a) -> IO a
withThread body = bracket (forkIO body) killThread
