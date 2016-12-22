{-# LANGUAGE GADTs, LambdaCase, ParallelListComp #-}
module Main where
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

fibs :: [Program Int]
fibs = replicate 39 step ++ [base]
  where
    base :: Program Int
    base = do
      send 0
      send 1
      return 1
    
    step :: Program Int
    step = do
      x1 <- receive
      x2 <- receive
      send x2
      let x3 = x1 + x2
      send x3
      return x3



runProgramF :: TChan Int
            -> TChan Int
            -> TChan Int
            -> ProgramF a
            -> IO a
runProgramF _ me _ (Send n) = atomically $ writeTChan me n
runProgramF l _  r Receive  = atomically $ do
  tryReadTChan l >>= \case
    Just x  -> return x
    Nothing -> readTChan r





runPrograms :: [Program a] -> IO [a]
runPrograms progs = do
  channels <- replicateM (length progs + 2) $ atomically newTChan
  outputs <- replicateM (length progs) newEmptyMVar
  sequence_ [ forkIO (runProgram l me r o p)
            | l  <- channels
            | me <- drop 1 channels
            | r  <- drop 2 channels
            | o  <- outputs
            | p  <- progs
            ]
  mapM takeMVar outputs

runProgram :: TChan Int
           -> TChan Int
           -> TChan Int
           -> MVar a
           -> Program a
           -> IO ()
runProgram _ _  _ o (Return x)     = putMVar o x
runProgram l me r o (Bind prog cc) = do
  x <- runProgramF l me r prog
  runProgram l me r o (cc x)



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
main = runPrograms fibs >>= print
