{-# LANGUAGE QuasiQuotes, RankNTypes, RecordWildCards, TemplateHaskell #-}
module Main where
import Control.Concurrent.Async
import Foreign
import qualified Language.C.Inline as C

newtype IORef a = IORef { ptrPtr :: Ptr (Ptr ()) }

newIORef :: a -> IO (IORef a)
newIORef x = do
  stableX <- newStablePtr x
  IORef <$> new (castStablePtrToPtr stableX)

readIORef :: IORef a -> IO a
readIORef (IORef {..}) = do
  stableX <- castPtrToStablePtr <$> peek ptrPtr
  deRefStablePtr stableX

main :: IO ()
main = printUniqueResults [] $ do
  ref <- newIORef (0 :: Int)
  tA <- async $ atomicModifyIORef ref (\x -> (x+1, ()))
  tB <- async $ atomicModifyIORef ref (\x -> (x+1, ()))
  mapM_ wait [tA,tB]
  readIORef ref



printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body



atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef ref@(IORef {..}) f = do
  stableExpected <- castPtrToStablePtr <$> peek ptrPtr
  (x', y) <- f <$> deRefStablePtr stableExpected
  stableReplacement <- newStablePtr x'
  r <- compareAndSwap ptrPtr (castStablePtrToPtr stableExpected)
                             (castStablePtrToPtr stableReplacement)
  if r then do freeStablePtr stableExpected
               return y
       else do freeStablePtr stableReplacement
               atomicModifyIORef ref f

compareAndSwap :: Ptr (Ptr ()) -> Ptr () -> Ptr () -> IO Bool
compareAndSwap ptr expected replacement = do
  r <- [C.block| int {
         return __sync_bool_compare_and_swap(
           $(void** ptr),
           $(void* expected),
           $(void* replacement)
         );
       }|]
  return (r /= 0)
