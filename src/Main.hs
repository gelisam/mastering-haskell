{-# LANGUAGE LambdaCase #-}
module Main where



wait :: Async a -> Process (Maybe a)
wait asyncX = realWait asyncX >>= \case
  Success   x -> return (Just x)
  Exception e -> error e
  LostContact -> return Nothing


attempt1 :: Process (Maybe Int)
attempt2 :: Process (Maybe Int)

tryYourBest :: Process Int
tryYourBest = attempt1 `orElse` attempt2 `orElse` return 0

infixr 3 `orElse`
orElse :: Process (Maybe a) -> Process a -> Process a
orElse body fallback = body >>= \case
  Just x  -> return x
  Nothing -> fallback



attempt1 = undefined
attempt2 = undefined



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
