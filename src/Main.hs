module Main where
import Control.Arrow
import Control.Exception.Base
import Data.Typeable

-- |
-- >>> catchType $ evaluate divByZero
-- Left ArithException
divByZero :: Int
divByZero = 1 `div` 0

-- |
-- >>> catchType $ evaluate (buggySum [1,2,3])
-- Left PatternMatchFail
buggySum :: [Int] -> Int
buggySum (x:xs) = x + buggySum xs
  -- [] case is missing

-- Only detected in compiled mode:
-- 
-- >>> catchType $ evaluate cyclic
-- Left NonTermination
cyclic :: Int
cyclic = 1 + cyclic

































































































typeOfException :: SomeException -> TypeRep
typeOfException (SomeException e) = typeOf e

catchType :: IO a -> IO (Either TypeRep a)
catchType = fmap (left typeOfException) . try




main :: IO ()
main = return ()
