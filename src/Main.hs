module Main where
import Control.Concurrent as Concurrent
import Foreign
import System.Mem
import System.Mem.Weak

main :: IO ()
main = do
  ptr <- mallocBytes 1024
  let freePtr = do putStrLn $ "freeing " ++ show ptr
                   free ptr
  weak <- mkWeakPtr ptr (Just freePtr)
  
  
  performGC >> Concurrent.yield
  
  putStrLn $ "still referenced"
  deRefWeak weak >>= print
  poke ptr (42 :: Int)
  
  performGC >> Concurrent.yield
  
  putStrLn "no longer referenced"
  deRefWeak weak >>= print
