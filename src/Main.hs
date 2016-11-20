module Main where

data Statement = PutStrLn String deriving Show
type Logging = [Statement]

-- |
-- >>> testStatement $ PutStrLn "hello"
-- ["hello"]
testStatement :: Statement -> [String]
testStatement (PutStrLn s) = [s]

-- |
-- >>> testLogging $ replicate 3 (PutStrLn "hello")
-- ["hello","hello","hello"]
testLogging :: Logging -> [String]
testLogging = foldMap testStatement


































































































main :: IO ()
main = return ()
