{-# LANGUAGE GADTs, LambdaCase #-}
module Main where
import Control.Concurrent.Async

main :: IO ()
main = do
  r1 <- async $ rpcInParallel [Acquire, Process, Archive]
  r2 <- async $ rpcInParallel [Process, Process, Process]
  r3 <- async $ rpcInParallel [Process, Process, Process]
  mapM_ wait [r1,r2,r3]

data Task where
  Acquire :: Task
  Process :: Task
  Archive :: Task

inParallel :: [Task] -> IO ()
inParallel tasks = do
  ts <- forM tasks $ \case
    Acquire -> async acquire
    Process -> async process
    Archive -> async archive
  mapM_ wait ts



rpcInParallel :: [Task] -> IO ()
rpcInParallel _ = return ()



acquire :: IO ()
acquire = return ()

process :: IO ()
process = return ()

archive :: IO ()
archive = return ()



forM :: [a] -> (a -> IO b) -> IO [b]
forM = flip mapM
