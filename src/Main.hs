module Main where
import Control.Monad
import Control.Concurrent.Async






main :: IO ()
main = do
  forM_ [return (42 :: Int), undefined] $ \body -> do
    asyncX <- async body
    
    putStrLn "waiting:"
    print =<< wait asyncX
    putStrLn "waited"
