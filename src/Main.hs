{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Monad.Trans.Maybe


wait :: Async a -> MaybeT Process a
wait asyncX = realWait asyncX >>= \case
  Success   x -> return x
  Exception e -> error e
  LostContact -> empty


attempt1 :: MaybeT Process Int
attempt2 :: MaybeT Process Int

tryYourBest :: MaybeT Process Int
tryYourBest = attempt1 <|> attempt2 <|> return 0

(<|>) :: MaybeT Process a -> MaybeT Process a -> MaybeT Process a







MaybeT mMaybeX <|> MaybeT mMaybeX' = MaybeT $ do
  mMaybeX >>= \case
    Just x  -> return (Just x)
    Nothing -> mMaybeX'
  
empty :: MaybeT Process a
empty = MaybeT $ return Nothing

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

realWait :: Async a -> MaybeT Process (Result a)
realWait = undefined



main :: IO ()
main = return ()
