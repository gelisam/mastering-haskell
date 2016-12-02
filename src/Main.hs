module Main where
import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Data.IORef

repeatT :: Int -> PTransform u u
repeatT n = PTransform $ \u pushU ->
            replicateM_ n (pushU u)

batchesOf :: Int -> IO (PTransform u [u])
batchesOf n = do
  ref <- newIORef []
  return $ PTransform $ \u pushList -> do
    modifyIORef ref (++ [u])
    us <- readIORef ref
    when (length us == n) $ do
      writeIORef ref []
      pushList us












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
