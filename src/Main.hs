module Main where
import Control.Arrow
import Control.Exception.Base
import Data.Typeable


typeOfException :: SomeException -> TypeRep
typeOfException (SomeException e) = typeOf e

catchType :: IO a -> IO (Either TypeRep a)
catchType = fmap (left typeOfException) . try


-- |
-- >>> try example :: IO (Either ArithException Int)
-- *** Exception: oops
-- 
-- >>> catchType example
-- Left AssertionFailed
-- 
-- >>> try example :: IO (Either AssertionFailed Int)
-- Left oops
example :: IO Int
example = throwIO $ AssertionFailed "oops"






































































































main :: IO ()
main = return ()
