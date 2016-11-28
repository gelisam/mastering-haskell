{-# LANGUAGE QuasiQuotes #-}
module Main where
import Control.Lens ((^.))
import qualified Language.C.Inline as C
import qualified Network.Wreq as Wreq
import qualified System.Process as Process

main :: IO ()
main = do
  n <- [C.block| int {
    int n = 1;
    for(int i=0; i<10; ++i) {
      n *= 2;
    }
    return n;
  }|]
  print n
  
  _ <- Process.system "cowsay moo"
  
  response <- Wreq.get "http://httpbin.org/status/200"
  print (response ^. Wreq.responseStatus)
