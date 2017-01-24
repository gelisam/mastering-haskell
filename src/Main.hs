module Main where
import Control.Concurrent.Async
import Data.IORef



myAtomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
myAtomicModifyIORef ref f = do
  (x', y) <- f <$> readIORef ref
  writeIORef ref x'
  return y






main :: IO ()
main = printUniqueResults [] $ do
  ref <- newIORef (0 :: Int)
  tA <- async $ myAtomicModifyIORef ref (\x -> (x+1, ()))
  tB <- async $ myAtomicModifyIORef ref (\x -> (x+1, ()))
  mapM_ wait [tA,tB]
  readIORef ref



printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body
