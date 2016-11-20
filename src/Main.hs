module Main where

data Statement = PutStrLn String deriving Show
type Logging = [Statement]

-- |
-- >>> runStatement $ PutStrLn "hello"
-- hello
runStatement :: Statement -> IO ()
runStatement (PutStrLn s) = putStrLn s

-- |
-- >>> runLogging $ replicate 3 (PutStrLn "hello")
-- hello
-- hello
-- hello
runLogging :: Logging -> IO ()
runLogging = mapM_ runStatement


































































































main :: IO ()
main = return ()
