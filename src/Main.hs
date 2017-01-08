{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Concurrent


data LState a = Empty | Full a
type LVar a = MVar (LState a)

newLVar :: IO (LVar a)
newLVar = newMVar Empty

readLVar :: LVar a -> IO a
readLVar var = readMVar var >>= \case
                 Full x -> return x
                 Empty  -> do yield
                              readLVar var

putLVar :: Eq a => LVar a -> a -> IO ()
putLVar var x = takeMVar var >>= \case
                  Full x' | x == x' -> putMVar var (Full x)
                  Empty             -> putMVar var (Full x)
                  _                 -> fail "incompatible put"











main :: IO ()
main = return ()
