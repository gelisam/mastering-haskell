module Main where
import Control.Arrow
import Control.Exception.Base
import Data.Typeable

-- |
-- >>> catchType $ evaluate example1
-- Left AssertionFailed
example1 :: Int
example1 = 1 + throw (AssertionFailed "oops")

-- |
-- >>> catchType $ evaluate example2
-- Left ErrorCall
example2 :: Int
example2 = 1 + error "oops"

-- |
-- >>> catchType $ evaluate example3
-- Left ErrorCall
example3 :: Int
example3 = 1 + undefined


































































































typeOfException :: SomeException -> TypeRep
typeOfException (SomeException e) = typeOf e

catchType :: IO a -> IO (Either TypeRep a)
catchType = fmap (left typeOfException) . try




main :: IO ()
main = return ()
