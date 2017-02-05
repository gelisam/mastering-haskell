module Main where
import Control.Concurrent.Async

main :: IO ()
main = do
  r1 <- remoteAsync $ do t1 <- async acquire
                         t2 <- async process
                         t3 <- async archive
                         mapM_ wait [t1,t2,t3]
  r2 <- remoteAsync $ do t1 <- async process
                         t2 <- async process
                         t3 <- async process
                         mapM_ wait [t1,t2,t3]
  r3 <- remoteAsync $ do t1 <- async process
                         t2 <- async process
                         t3 <- async process
                         mapM_ wait [t1,t2,t3]
  mapM_ wait [r1,r2,r3]
















acquire :: IO ()
acquire = return ()

process :: IO ()
process = return ()

archive :: IO ()
archive = return ()


remoteAsync :: IO a -> IO (Async a)
remoteAsync = async
