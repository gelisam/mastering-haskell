{-# LANGUAGE GADTs #-}
module Main where

data ProgramF a where
  Send    :: Int -> ProgramF ()
  Receive :: ProgramF Int

data Program a where
  Return :: a -> Program a
  Bind   :: ProgramF a -> (a -> Program b) -> Program b

send :: Int -> Program ()
send n = Bind (Send n) Return

receive :: Program Int
receive = Bind Receive Return


runPrograms :: [Program a] -> IO [a]
runPrograms = undefined







main :: IO ()
main = return ()
