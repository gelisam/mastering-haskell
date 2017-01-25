{-# LANGUAGE QuasiQuotes, RankNTypes, TemplateHaskell #-}
module Main where
import Control.Concurrent.Async
import Foreign
import qualified Language.C.Inline as C

compareAndSwap :: Ptr C.CInt -> C.CInt -> C.CInt -> IO Bool
compareAndSwap ptr expected replacement = do
  r <- [C.block| int {
         return __sync_bool_compare_and_swap(
           $(int* ptr),
           $(int expected),
           $(int replacement)
         );
       }|]
  return (r /= 0)



main :: IO ()
main = printUniqueResults [] $ with 0 $ \ptr -> do
  tA <- async $ compareAndSwap ptr 0 1
  tB <- async $ compareAndSwap ptr 0 1
  mapM wait [tA,tB]



printUniqueResults :: (Show a, Eq a) => [a] -> IO a -> IO ()
printUniqueResults seen body = do
  x <- body
  if x `elem` seen then printUniqueResults seen body
                   else print x >> printUniqueResults (x:seen) body
