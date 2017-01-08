module Main where
import Control.Concurrent
import Control.Monad

fib :: LVar Integer -> Int -> IO ()
fib var 0 = incrementLVar var
fib var 1 = incrementLVar var
fib var n = fib var (n-1) >> fib var (n-2)

main :: IO ()
main = forM_ [10,20,30,40] $ \n -> do
  print =<< isFibLarge n

isFibLarge :: Int -> IO Bool
isFibLarge n = do
  var <- newLVar 0
  raceL (True <$ waitUntilL 100 var)
        (do fib var n
            (> 100) <$> freezeLVar var)







data LState a = Growing a | Frozen a
type LVar a = MVar (LState a)

newLVar :: a -> IO (LVar a)
newLVar = newMVar . Growing

incrementLVar :: Num a => LVar a -> IO ()
incrementLVar var = takeMVar var >>= \r -> case r of
  Growing x -> putMVar var (Growing (x+1))
  Frozen _  -> fail "frozen var"

waitUntilL :: Ord a => a -> LVar a -> IO ()
waitUntilL n var = readMVar var >>= \r -> case r of
  Growing x | x >= n -> return ()
  Frozen  x | x >= n -> return ()
  _                  -> yield >> waitUntilL n var

freezeLVar :: LVar a -> IO a
freezeLVar var = takeMVar var >>= \r -> case r of
  Growing x -> putMVar var (Frozen x) >> return x
  Frozen  x -> putMVar var (Frozen x) >> return x



-- "LVar" now has two distinct meanings,
-- which I'm going to merge shortly

data LState' a = Empty | Full a
type LVar' a = MVar (LState' a)

newLVar' :: IO (LVar' a)
newLVar' = newMVar Empty

readLVar' :: LVar' a -> IO a
readLVar' var = readMVar var >>= \r -> case r of
                  Full x -> return x
                  Empty  -> do yield
                               readLVar' var

putLVar' :: Eq a => LVar' a -> a -> IO ()
putLVar' var x = takeMVar var >>= \r -> case r of
                   Full x' | x == x' -> putMVar var (Full x)
                   Empty             -> putMVar var (Full x)
                   _                 -> fail "incompatible put"


raceL :: Eq a => IO a -> IO a -> IO a
raceL ioX1 ioX2 = do
  var <- newLVar'
  _ <- forkIO $ do x1 <- ioX1; x1 `seq` putLVar' var x1
  _ <- forkIO $ do x2 <- ioX2; x2 `seq` putLVar' var x2
  readLVar' var
