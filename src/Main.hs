{-# LANGUAGE GADTs #-}
module Main where

data CmdI a r where
  ReadI ::      CmdI a a
  Write :: a -> CmdI a ()













data CmdM a r where
  WaitM  :: (a -> Bool) -> CmdM a ()
  Modify :: (a -> a)    -> CmdM a ()



main :: IO ()
main = return ()
