{-# LANGUAGE GADTs, ParallelListComp #-}
module Main where
import Control.Concurrent
import Control.Monad

runPrograms :: [Program a] -> IO [a]
runPrograms progs = do
  channels <- replicateM (length progs + 1) newChan
  outputs <- replicateM (length progs) newEmptyMVar
  sequence_ [forkIO (runProgram l r o p) | l <- channels
                                         | r <- tail channels
                                         | o <- outputs
                                         | p <- progs
                                         ]
  mapM takeMVar outputs

runProgram :: Chan Int -> Chan Int -> MVar a -> Program a -> IO ()
runProgram _ _ o (Return x)     = putMVar o x
runProgram l r o (Bind prog cc) = do x <- runProgramF l r prog
                                     runProgram l r o (cc x)

runProgramF :: Chan Int -> Chan Int -> ProgramF a -> IO a
runProgramF _ r (Send n) = writeChan r n
runProgramF l _ Receive  = readChan l





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


instance Functor Program where
  fmap f (Return x)   = Return (f x)
  fmap f (Bind px cc) = Bind px (fmap f <$> cc)

instance Applicative Program where
  pure = Return
  (<*>) = ap

instance Monad Program where
  Return x   >>= f = f x
  Bind px cc >>= f = Bind px ((>>= f) <$> cc)


main :: IO ()
main = return ()
