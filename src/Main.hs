{-# LANGUAGE QuasiQuotes, RankNTypes, TemplateHaskell #-}
module Main where
import Foreign
import qualified Language.C.Inline as C


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
