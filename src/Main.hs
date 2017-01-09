{-# LANGUAGE GADTs #-}
module Main where

data CmdI a r where
  ReadI ::      CmdI a a
  Write :: a -> CmdI a ()

data CmdF a r where
  FreezeF ::      CmdF a a
  AddF    :: a -> CmdF a ()

data CmdL k a r where
  LookupL :: k ->      CmdL k a a
  InsertL :: k -> a -> CmdL k a ()

data CmdC a r where
  RegisterC :: (a -> IO ()) -> CmdC a ()
  InsertC   :: a -> CmdC a ()

data CmdM a r where
  WaitM  :: (a -> Bool) -> CmdM a ()
  Modify :: (a -> a)    -> CmdM a ()



main :: IO ()
main = return ()
