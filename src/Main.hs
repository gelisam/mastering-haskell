module Main where



newtype PTransform u v = PTransform
  { onPush :: u
           -> (v -> IO ())
           -> IO ()
  }






















main :: IO ()
main = return ()
