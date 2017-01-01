{-# LANGUAGE BangPatterns, RecordWildCards #-}
module Main where
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.IO

main :: IO ()
main = do
  counter1 <- newCounter
  counter2 <- newCounter
  forever $ do runThreads [ do takeMVar (mutex counter1)
                               takeMVar (mutex counter2)
                               putMVar  (mutex counter2) ()
                               putMVar  (mutex counter1) ()
                          , do takeMVar (mutex counter2)
                               takeMVar (mutex counter1)
                               putMVar  (mutex counter1) ()
                               putMVar  (mutex counter2) ()
                          ]
               printCounter counter1











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



data Counter = Counter { mutex  :: MVar ()
                       , field1 :: IORef Int
                       , field2 :: IORef Int
                       }

newCounter :: IO Counter
newCounter = Counter <$> newMVar () <*> newIORef 0 <*> newIORef 0

assertInvariant :: Counter -> IO ()
assertInvariant (Counter {..}) = do r <- (==) <$> readIORef field1
                                              <*> readIORef field2
                                    when (not r) $ fail "violation"

increment :: Counter -> IO ()
increment c@(Counter {..}) = do takeMVar mutex
                                assertInvariant c
                                modifyIORef' field1 (+1)
                                modifyIORef' field2 (+1)
                                assertInvariant c
                                putMVar mutex ()
