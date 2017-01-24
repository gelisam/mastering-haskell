{-# LANGUAGE QuasiQuotes, RankNTypes, RecordWildCards, TemplateHaskell #-}
module Main where
import Foreign
import qualified Language.C.Inline as C

newtype IORef a = IORef { ptrPtr :: Ptr (Ptr ()) }

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

main :: IO ()
main = return ()
