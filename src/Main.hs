module Main where
import Control.Concurrent.Async
import Data.IORef



compareAndSwap :: Eq a => IORef a -> a -> a -> IO Bool
compareAndSwap ref expected replacement = do
  actual <- readIORef ref
  if actual == expected then do writeIORef ref replacement
                                return True
                        else return False






main :: IO ()
main = printUniqueResults [] $ do
  ref <- newIORef (0 :: Int)
  tA <- async $ compareAndSwap ref 0 1
  tB <- async $ compareAndSwap ref 0 1
  mapM wait [tA,tB]



printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body
