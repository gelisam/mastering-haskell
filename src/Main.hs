module Main where

putStrLn :: String -> Logging ()

runLogging :: Logging a -> Console a
getLine    :: Console String

runConsole :: Console a -> IO a
