module Main where
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO hiding (withFile)
import System.Timeout


withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile filePath mode = bracket open close
  where
    open = openFile filePath mode
    close h = do
      hClose h
      putStrLn "closed the file."

-- |
-- >>> timeout 1000000 example
-- closed the file.
-- Nothing
example :: IO a
example = withFile "foo.txt" ReadMode (\h -> sleepForever)



























































































sleepForever :: IO a
sleepForever = forever
             $ threadDelay 2000000


-- prevent a warning about unsued System.Timeout
timeout' :: Int -> IO a -> IO (Maybe a)
timeout' = timeout


main :: IO ()
main = return ()
