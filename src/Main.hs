{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Concurrent

data LState a = Growing a | Frozen a
type LVar a = MVar (LState a)

newLVar :: a -> IO (LVar a)
newLVar = newMVar . Growing

waitUntilL :: Ord a => a -> LVar a -> IO ()
waitUntilL n var = readMVar var >>= \r -> case r of
  Growing x | x >= n -> return ()
  Frozen  x | x >= n -> return ()
  _                  -> yield >> waitUntilL n var

incrementLVar :: Num a => LVar a -> IO ()
incrementLVar var = takeMVar var >>= \case
  Growing x -> putMVar var (Growing (x+1))
  Frozen _  -> fail "frozen var"

freezeLVar :: LVar a -> IO a
freezeLVar var = takeMVar var >>= \case
  Growing x -> putMVar var (Frozen x) >> return x
  Frozen  x -> putMVar var (Frozen x) >> return x








main :: IO ()
main = return ()
