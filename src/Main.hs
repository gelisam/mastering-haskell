module Main where
import Prelude hiding (id, (.))
import Control.Category

newtype PTransform u v = PTransform
  { onPull :: IO u -> IO v
  }

instance Category PTransform where
  id = PTransform $ \pullU -> pullU
  t2 . t1 = PTransform $ \pullU ->
            onPull t2 (onPull t1 pullU)













main :: IO ()
main = return ()
