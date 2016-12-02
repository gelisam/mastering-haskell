module Main where
import Prelude hiding (id, (.))
import Control.Category

newtype PTransform u v = PTransform
  { onPush :: u
           -> (v -> IO ())
           -> IO ()
  }

instance Category PTransform where
  id = PTransform $ \u pushU -> pushU u
  t2 . t1 = PTransform $ \u pushW ->
            onPush t1 u $ \v ->
            onPush t2 v $ \w ->
            pushW w















main :: IO ()
main = return ()
