module Main where
import Control.Exception.Base


-- |
-- >>> example1
-- *** Exception: oops
example1 :: IO Int
example1 = throwIO $ AssertionFailed "oops"

-- |
-- >>> example2
-- Left oops
example2 :: IO (Either AssertionFailed a)
example2 = try $ throwIO $ AssertionFailed "oops"

-- |
-- >>> example3
-- *** Exception: oops
example3 :: IO (Either ArithException a)
example3 = try $ throwIO $ AssertionFailed "oops"






































































































main :: IO ()
main = return ()
