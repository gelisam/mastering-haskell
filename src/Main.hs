{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import Control.Monad
import System.IO.Unsafe



cache :: [Integer]
cache =       take 10 $ 1:1:zipWith noisyPlus cache (tail cache)



fib :: Int -> IO Integer
fib     n | n <  10   = return (cache !! n)
          | otherwise = traceThread "fib"
                      $ (+) <$> fib     (n-1) <*> fib     (n-2)

main :: IO ()
main = traceThread "main" $ do
  
  
  runThreads $ [fib     11, fib     12]









traceThread :: String -> a -> a
traceThread msg x = unsafePerformIO $ do
  threadId <- myThreadId
  putStrLn $ "[" ++ show threadId ++ "] " ++ msg
  return x

noisyPlus :: Num a => a -> a -> a
noisyPlus x y = traceThread "cache" $ x + y

runThreads :: [IO a] -> IO ()
runThreads threads = do
  -- spawn a dummy thread so that the thread numbers which
  -- compute fib are the same in this slide and the next
  void $ forkIO $ return ()
  
  vars <- forM threads $ \thread -> do
    var <- newEmptyMVar
    void $ forkFinally (do {!_ <- thread; putMVar var (Right ())})
                       (putMVar var)
    return var
  rs <- mapM takeMVar vars
  forM_ rs $ \r -> case r of
    Left e -> error (show e)
    Right _ -> return ()



type IVar a = MVar a

newIVar :: IO (IVar a)
newIVar = newEmptyMVar

readIVar :: IVar a -> IO a
readIVar var = readMVar var

putIVar :: IVar a -> a -> IO ()
putIVar var x = do r <- tryPutMVar var x
                   when (not r) $ fail "double put"
