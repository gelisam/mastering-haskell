module Main where
import Control.Concurrent.Async

data AsyncList a = Cons a (Async (AsyncList a))
data MsgQueue  a = Cons' a (IVar (MsgQueue a))






















data IVar a



main :: IO ()
main = return ()
