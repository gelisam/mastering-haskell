{-# LANGUAGE BangPatterns, RecordWildCards #-}
module Main where
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.IO

main :: IO ()
main = do
  counter <- newCounter
  forever $ do runThreads $ replicate 2 $ increment counter
               printCounter counter

data Counter = Counter { mutex  :: MVar ()
                       , field1 :: IORef Int
                       , field2 :: IORef Int
                       }

increment :: Counter -> IO ()
increment c@(Counter {..}) = do takeMVar mutex
                                assertInvariant c
                                modifyIORef' field1 (+1)
                                modifyIORef' field2 (+1)
                                assertInvariant c
                                putMVar mutex ()










printCounter :: Counter -> IO ()
printCounter (Counter {..}) = do
  n <- readIORef field1
  putStr $ show n ++ ", "
  hFlush stdout
  threadDelay (200 * 1000)

runThreads :: [IO a] -> IO ()
runThreads threads = do
  vars <- forM threads $ \thread -> do
    var <- newEmptyMVar
    void $ forkFinally (do {!_ <- thread; putMVar var (Right ())})
                       (putMVar var)
    return var
  rs <- mapM takeMVar vars
  forM_ rs $ \r -> case r of
    Left e -> error (show e)
    Right _ -> return ()


newCounter :: IO Counter
newCounter = Counter <$> newMVar () <*> newIORef 0 <*> newIORef 0

assertInvariant :: Counter -> IO ()
assertInvariant (Counter {..}) = do r <- (==) <$> readIORef field1
                                              <*> readIORef field2
                                    when (not r) $ fail "violation"
