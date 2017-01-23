module Main where

-- |
-- >>> import Control.Concurrent
-- >>> :t modifyMVar
-- modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
-- 
-- >>> import Data.IORef
-- >>> :t atomicModifyIORef
-- atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
main :: IO ()
main = return ()
