module Main where
import Control.Concurrent as Concurrent
import Foreign.Concurrent as Concurrent
import Foreign
import System.Mem

main :: IO ()
main = do
  ptr <- mallocBytes 1024
  let freePtr = do putStrLn $ "freeing " ++ show ptr
                   free ptr
  foreignPtr <- Concurrent.newForeignPtr ptr freePtr
  
  
  performGC >> Concurrent.yield
  
  putStrLn $ "still referenced"
  withForeignPtr foreignPtr $ flip poke (42 :: Int)
  
  
  performGC >> Concurrent.yield
  
  putStrLn "no longer referenced"
