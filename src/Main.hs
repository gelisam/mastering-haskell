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
      Nothing -> decreaseLocalInventory p

sync :: Process a
sync = forever $ do
  liftIO $ sleep 20
  getRemoteInventory >>= \case
    Just xsR -> do
      xsL <- getLocalInventory
      let xs' = merge xsL xsR
      applyDiffLocal  (diff xsL xs')
      applyDiffRemote (diff xsR xs')  -- potential conflict!
    Nothing -> sync



















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

decreaseLocalInventory :: ProductId -> Process ()
decreaseLocalInventory = undefined

decreaseRemoteInventory :: ProductId -> Process (Maybe ())
decreaseRemoteInventory = undefined


data Inventory

merge :: Inventory -> Inventory -> Inventory
merge = undefined

getLocalInventory :: Process Inventory
getLocalInventory = undefined

getRemoteInventory :: Process (Maybe Inventory)
getRemoteInventory = undefined


data InventoryDiff

diff :: Inventory -> Inventory -> InventoryDiff
diff = undefined

applyDiffLocal :: InventoryDiff -> Process ()
applyDiffLocal = undefined

applyDiffRemote :: InventoryDiff -> Process ()
applyDiffRemote = undefined



sleep :: Double -> IO ()
sleep = undefined

liftIO :: IO a -> Process a
liftIO = undefined

forever :: Monad m => m () -> m a
forever body = body >> forever body



main :: IO ()
main = return ()
