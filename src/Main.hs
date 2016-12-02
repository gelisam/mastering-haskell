module Main where
import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Data.IORef

batchesOf :: Int -> PTransform u [u]
batchesOf n = PTransform $ \pullU -> replicateM n pullU

repeatT :: Int -> IO (PTransform u u)
repeatT n = do
  ref <- newIORef Nothing
  return $ PTransform $ \pullU -> do
    r <- readIORef ref
    when (fmap fst r == Nothing || fmap fst r == Just 0) $ do
      u <- pullU
      writeIORef ref $ Just (n, u)
    Just (i, u) <- readIORef ref
    writeIORef ref $ Just (i-1, u)
    return u








newtype PTransform u v = PTransform
  { onPull :: IO u -> IO v
  }

instance Category PTransform where
  id = PTransform $ \pullU -> pullU
  t2 . t1 = PTransform $ \pullU ->
            onPull t2 (onPull t1 pullU)


main :: IO ()
main = return ()
