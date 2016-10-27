module Main where

putStrLn :: String -> IO ()
getLine  :: IO String

readFile  :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()

newIORef   :: a -> IO (IORef a)
readIORef  :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()

forkIO :: IO () -> IO ThreadId
killThread :: ThreadId -> IO ()

threadDelay :: Int -> IO ()

try :: Exception e => IO a -> IO (Either e a)
finally :: IO a -> IO b -> IO a

newUnique :: IO Unique
makeStableName :: a -> IO (StableName a)

getArgs :: IO [String]
getEnv :: String -> IO String
exitSuccess :: IO a
exitFailure :: IO a

newCString :: String -> IO CString
peekCString :: CString -> IO String

performGC :: IO ()


