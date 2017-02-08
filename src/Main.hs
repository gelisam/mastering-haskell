{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Concurrent.Async

main :: IO ()
main = do
  r1 <- async $ rpcInParallel ["acquire", "process", "archive"]
  r2 <- async $ rpcInParallel ["process", "process", "process"]
  r3 <- async $ rpcInParallel ["process", "process", "process"]
  mapM_ wait [r1,r2,r3]


rpcInParallel :: [String] -> IO ()

---

inParallel :: [String] -> IO ()
inParallel tasks = do
  ts <- forM tasks $ \case
    "acquire" -> async acquire
    "process" -> async process
    "archive" -> async archive
    _         -> error "unknown task"
  mapM_ wait ts



rpcInParallel _ = return ()



acquire :: IO ()
acquire = return ()

process :: IO ()
process = return ()

archive :: IO ()
archive = return ()



forM :: [a] -> (a -> IO b) -> IO [b]
forM = flip mapM
