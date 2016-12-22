{-# LANGUAGE GADTs, ParallelListComp #-}
module Main where
import Control.Concurrent
import Control.Monad

fibs :: [Program [Int]]
fibs = [source, transform, sink]
  where
    source, transform, sink :: Program [Int]
    source = do mapM_ send [0..10]
                send (-1)
                return [0..10]
    transform = do
      n <- receive
      if n == (-1) then do send (-1)
                           return []
                   else if even n then do send n
                                          (n:) <$> transform
                                  else transform
    sink = do
      n <- receive
      if n == (-1) then return [0]
                   else do s <- head <$> sink
                           return [n + s]

main :: IO ()
main = runPrograms fibs >>= mapM_ print



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
