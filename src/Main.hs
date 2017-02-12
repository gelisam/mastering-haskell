{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Monad.Trans.Maybe

buyProduct :: ProductId -> Process ()
buyProduct p = do -- TODO: charge credit card, ship the item...
                  decreaseInventory
  where
    decreaseInventory :: Process ()
    decreaseInventory = decreaseRemoteInventory p >>= \case
      Just () -> return ()
      Nothing -> return ()





























data Process a

instance Functor Process where
  fmap = undefined

instance Applicative Process where
  pure  = undefined
  (<*>) = undefined

instance Monad Process where
  (>>=) = undefined


class Monad m => MonadProcess m where
  liftProcess :: Process a -> m a

instance MonadProcess Process where
  liftProcess = id

instance MonadProcess m => MonadProcess (MaybeT m) where
  liftProcess px = MaybeT (Just <$> liftProcess px)



data ProductId

decreaseRemoteInventory :: ProductId -> Process (Maybe ())
decreaseRemoteInventory = undefined



sleep :: Double -> IO ()
sleep = undefined

liftIO :: IO a -> Process a
liftIO = undefined



main :: IO ()
main = return ()
