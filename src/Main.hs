module Main where
import Control.Arrow
import Control.Concurrent
import Control.Exception.Base
import Control.Monad
import Data.Typeable
import System.Timeout


-- |
-- >>> timeout 1000000 sleepForever
-- Nothing 
sleepForever :: IO a
sleepForever = forever
             $ threadDelay 2000000

-- |
-- >>> timeout 1000000 uninterruptibleSleep
-- ...
uninterruptibleSleep :: IO a
uninterruptibleSleep = forever
                     $ catchType $ threadDelay 2000000
































































































typeOfException :: SomeException -> TypeRep
typeOfException (SomeException e) = typeOf e

catchType :: IO a -> IO (Either TypeRep a)
catchType = fmap (left typeOfException) . try


-- prevent a warning about unsued System.Timeout
timeout' :: Int -> IO a -> IO (Maybe a)
timeout' = timeout


main :: IO ()
main = return ()
