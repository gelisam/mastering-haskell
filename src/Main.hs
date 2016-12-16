module Main where

import Control.Monad.ST
import Data.STRef


newIntRef :: ST s (STRef s Int)
newIntRef = newSTRef 42

incrIntRef :: STRef s Int -> ST s ()
incrIntRef ref = modifySTRef ref (+1)




valid :: ST s Int
valid = do
  ref <- newIntRef
  incrIntRef ref
  readSTRef ref









main :: IO ()
main = return ()
