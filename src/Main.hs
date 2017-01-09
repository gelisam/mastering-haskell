module Main where


data CmdI a = ReadI      | WriteI a
data CmdF   = IncrementF | FreezeF
data CmdE   = IncrementE | ReadEvenE

class LVish c
instance LVish (CmdI a)
instance LVish CmdF
--instance LVish CmdE



















main :: IO ()
main = return ()
