module Main where
import Numeric.Natural

-- IVar with double put
data CmdI a = Read   -- block until Put
            | Put a  -- conflicts with (Put x') if x /= x'









































main :: IO ()
main = return ()
