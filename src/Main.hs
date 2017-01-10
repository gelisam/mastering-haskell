module Main where
import Control.Concurrent
import Control.Exception.Base
import Data.Typeable

data TooSlow = TooSlow deriving (Show, Typeable)
instance Exception TooSlow

raceM :: IO a -> IO a -> IO a
raceM ioX1 ioX2 = do
    var <- newEmptyMVar
    [thread1,thread2] <- mapM (fork var) [ioX1,ioX2]
    r <- either throw id <$> takeMVar var
    mapM_ (flip throwTo TooSlow) [thread1, thread2]
    return r
  where
    fork :: MVar (Either SomeException a) -> IO a -> IO ThreadId
    fork var body = forkFinally body $ \r -> case r of
      Left (SomeException e) | typeOf e == typeOf TooSlow
        -> return ()
      _ -> putMVar var r

main :: IO ()
main = do print =<< parOr (fib 30 > 100) (fib 40 > 100)
          print =<< parOr (fib 40 > 100) (fib 30 > undefined)




parOr :: Bool -> Bool -> IO Bool
parOr b1 b2 = raceM (evaluate $ b1 || b2)
                    (evaluate $ b2 || b1)

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
