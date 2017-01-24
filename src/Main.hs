module Main where
import Data.IORef




compareAndSwap :: Eq a => IORef a -> a -> a -> IO Bool
compareAndSwap ref expected replacement = do
  actual <- readIORef ref
  if actual == expected then do writeIORef ref replacement
                                return True
                        else return False



main :: IO ()
main = do
  ref <- newIORef (42 :: Int)
  
  print =<< compareAndSwap ref 5 10
  print =<< readIORef ref
  
  print =<< compareAndSwap ref 42 50
  print =<< readIORef ref



