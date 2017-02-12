{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Applicative
import Control.Monad.Trans.Maybe

renderProductDetails :: ProductId -> Process HTML
renderProductDetails p = runMaybeT go >>= \case
    Just t  -> renderTemplate t
    Nothing -> renderText "This page is currently unavailable."
  where
    go :: MaybeT Process (Template HTML)
    go = productDetailsPage
     <$> getName p
     <*> (getImage p        <|> return "no-image-available.png")
     <*> (getDesc  p        <|> return "no description available")
     <*> (getCurrentPrice p <|> estimatePrice p)
     <*> (renderInventory   <|> return emptyDiv)
     <*> (renderReviews     <|> return emptyDiv)
     <*> (renderRelated     <|> return emptyDiv)
    
    renderInventory :: MaybeT Process HTML
    renderInventory = do
      n <- getInventoryCount p
      renderText $ "In stock! " ++ show n ++ " copies left."
    
    
    
    renderReviews :: MaybeT Process HTML
    renderReviews = undefined
    
    renderRelated :: MaybeT Process HTML
    renderRelated = undefined
    
    productDetailsPage :: String -> String -> String
                       -> Price
                       -> HTML -> HTML -> HTML
                       -> Template HTML
    productDetailsPage = undefined



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



data HTML

emptyDiv :: HTML
emptyDiv = undefined

renderText :: MonadProcess m => String -> m HTML
renderText = undefined


data Template a

instance Functor Template where
  fmap = undefined

instance Applicative Template where
  pure  = undefined
  (<*>) = undefined

renderTemplate :: MonadProcess m => Template a -> m a
renderTemplate = undefined


data ProductId

getName :: ProductId -> MaybeT Process String
getName = undefined

getImage :: ProductId -> MaybeT Process String
getImage = undefined

getDesc :: ProductId -> MaybeT Process String
getDesc = undefined

getInventoryCount :: ProductId -> MaybeT Process Int
getInventoryCount = undefined


data Price

getCurrentPrice :: ProductId -> MaybeT Process Price
getCurrentPrice = undefined

estimatePrice :: ProductId -> MaybeT Process Price
estimatePrice = undefined



main :: IO ()
main = return ()
