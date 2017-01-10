{-# LANGUAGE FlexibleContexts, GADTs #-}
module Main where
import Control.Concurrent
import Data.MultiSet as MultiSet

type LVar c = MVar (MultiSet (SomeR c))

newLVar :: IO (LVar a)
newLVar = newMVar MultiSet.empty

apply :: (LVish c, Ord (SomeR c)) => c a -> LVar c -> IO a
apply c var = do
  cs <- takeMVar var
  if isAllowed c cs
  then case result c cs of
         Just x  -> do putMVar var $ MultiSet.insert (SomeR c) cs
                       return x
         Nothing -> do putMVar var cs
                       yield
                       apply c var
  else fail "command not allowed"












implies :: Bool -> Bool -> Bool
implies x y = y || not x




class LVish c where
  isAllowed :: c r -> MultiSet (SomeR c) -> Bool
  result    :: c r -> MultiSet (SomeR c) -> Maybe r

data SomeR c where
  SomeR :: c r -> SomeR c



main :: IO ()
main = return ()
