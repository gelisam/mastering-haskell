{-# LANGUAGE GADTs #-}
module Main where

data CmdI a r where
  ReadI ::      CmdI a a
  Write :: a -> CmdI a ()



















main :: IO ()
main = return ()
