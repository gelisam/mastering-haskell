{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import System.IO.Unsafe

mkCache :: IVar [Integer] -> IO ()
mkCache var = traceThread "mkCache" $ do
  let cache = take 10 $ 1:1:zipWith noisyPlus cache (tail cache)
  let !_ = force cache
  putIVar var cache

fib :: IVar [Integer] -> Int -> IO Integer
fib var n | n <  10   = (!! n) <$> readIVar var
          | otherwise = traceThread "fib"
                      $ (+) <$> fib var (n-1) <*> fib var (n-2)

main :: IO ()
main = traceThread "main" $ do
  var <- newIVar
  void $ forkIO $ mkCache var
  runThreads $ [fib var 11, fib var 12]









traceThread :: String -> a -> a
traceThread msg x = unsafePerformIO $ do
  threadId <- myThreadId
  putStrLn $ "[" ++ show threadId ++ "] " ++ msg
  return x

noisyPlus :: Num a => a -> a -> a
noisyPlus x y = traceThread "cache" $ x + y

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



type IVar a = MVar a

newIVar :: IO (IVar a)
newIVar = newEmptyMVar

readIVar :: IVar a -> IO a
readIVar var = readMVar var

putIVar :: IVar a -> a -> IO ()
putIVar var x = do r <- tryPutMVar var x
                   when (not r) $ fail "double put"
