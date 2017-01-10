module Main where
import Numeric.Natural

-- IVar with double put
data CmdI a = Read   -- block until Put
            | Put a  -- conflicts with (Put x') if x /= x'

-- (Bool, Bool)
data CmdB = PutFst Bool  -- conflicts with (PutFst b') if b /= b'
          | PutSnd Bool  -- conflicts with (PutSnd b') if b /= b'
          | ReadFst      -- block until (PutFst b)
          | ReadSnd      -- block until (PutSnd b)
          | ReadAnd      -- block until one False or two Trues
          | ReadOr       -- block until one True or two Falses

-- Incrementing counter
data CmdF = Freeze
          | WaitUntil Natural    -- block until value is reached
          | IncrementBy Natural  -- error if after Freeze

-- Map k a
data CmdL k a = Lookup k    -- block until (Insert k)
              | Insert k a  -- conflicts with (Insert k' x')
                            -- if k == k' but x /= x'























main :: IO ()
main = return ()
