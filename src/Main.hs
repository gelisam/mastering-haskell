module Main where
import Prelude hiding (readFile)
import Control.Exception
import System.IO hiding (hGetContents, withFile, readFile)
import System.IO.Unsafe

main :: IO ()
main = do
  writeFile "foo.txt" "abc"
  
  s <- withFile "foo.txt" ReadMode $ \h -> do
    hGetContents h
  print s
















hGetContents :: Handle -> IO String
hGetContents h = do
  r <- hIsEOF h
  if r then do putStrLn "closing handle"
               hClose h
               return []
       else do x <- hGetChar h
               xs <- unsafeInterleaveIO $ hGetContents h
               return (x:xs)

readFile :: FilePath -> IO String
readFile filePath = do h <- openFile filePath ReadMode
                       hGetContents h

withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile filePath mode = bracket acquire release
  where acquire = openFile filePath mode
        release h = do putStrLn "releasing handle"
                       hClose h
