{-# LANGUAGE LambdaCase #-}
module Main where



wait :: Async a -> Process a
wait asyncX = realWait asyncX >>= \case
  Success   x -> return x
  Exception e -> error e
  LostContact -> do liftIO $ sleep 1
                    wait asyncX





















data Async a
data Process a
data Result a = Success a | Exception String | LostContact

instance Functor Process where
  fmap = undefined

instance Applicative Process where
  pure  = undefined
  (<*>) = undefined

instance Monad Process where
  (>>=) = undefined

realWait :: Async a -> Process (Result a)
realWait = undefined



sleep :: Double -> IO ()
sleep = undefined

liftIO :: IO a -> Process a
liftIO = undefined



main :: IO ()
main = return ()
