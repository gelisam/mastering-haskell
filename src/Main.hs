module Main where
import Control.Concurrent.Async

main :: IO ()
main = do
  t1 <- async acquire
  t2 <- async process
  t3 <- async archive
  mapM_ wait [t1,t2,t3]
  
















acquire :: IO ()
acquire = return ()

process :: IO ()
process = return ()

archive :: IO ()
archive = return ()
