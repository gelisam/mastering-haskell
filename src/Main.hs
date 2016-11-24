module Main where
import Control.Arrow
import Control.Exception.Base
import Data.Typeable

-- |
-- >>> example
-- *** Exception: oops
-- 
-- >>> Right x <- catchType $ return example
-- >>> print x
-- *** Exception: oops
-- 
-- >>> catchType $ example `seq` return 42
-- Left AssertionFailed
-- 
-- >>> catchType $ evaluate example
-- Left AssertionFailed
example :: Int
example = 1 + throw (AssertionFailed "oops")



































































































typeOfException :: SomeException -> TypeRep
typeOfException (SomeException e) = typeOf e

catchType :: IO a -> IO (Either TypeRep a)
catchType = fmap (left typeOfException) . try




main :: IO ()
main = return ()
