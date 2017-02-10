{-# LANGUAGE LambdaCase #-}
module Main where



wait :: Async a -> Process a
wait asyncX = realWait asyncX >>= \case
  Success   x -> return x
  Exception e -> error e
  LostContact -> error "lost contact"





















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



main :: IO ()
main = return ()
