module Main where
import Control.Concurrent
import Control.Monad


waitUntil :: (a -> Bool) -> MVar a -> IO ()
waitUntil p var = do x <- readMVar var
                     unless (p x) $ do yield
                                       waitUntil p var



















main :: IO ()
main = return ()
