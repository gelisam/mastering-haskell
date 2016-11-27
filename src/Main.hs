module Main where
import Control.Concurrent as Concurrent
import Foreign
import System.Mem
import System.Mem.Weak
import Data.IORef
main :: IO ()
main = do
  ptr <- mallocBytes 1024
  let freePtr = do putStrLn $ "freeing " ++ show ptr
                   free ptr
  ref <- newIORef ptr
  weak <- mkWeakIORef ref freePtr
  
  performGC >> Concurrent.yield
  
  putStrLn $ "still referenced"
  deRefWeak weak >>= traverse readIORef >>= print
  poke ptr (42 :: Int) >> modifyIORef ref id
  
  performGC >> Concurrent.yield
  
  putStrLn "no longer referenced"
  deRefWeak weak >>= traverse readIORef >>= print
