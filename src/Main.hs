{-# LANGUAGE RankNTypes #-}
module Main where

newtype ForallBehaviour a = ForallBehaviour
  { getBehaviour :: forall t. Behaviour t a }
newtype ForallEvent a = ForallEvent
  { getEvent :: forall t. Event t a }

toggle1 :: Behaviour s Bool
toggle1 = toggleB (buttonPressE button1)

switchEB' :: Behaviour s a
          -> Event s (ForallBehaviour a)
          -> Behaviour s a
switchEB' = undefined

switchEB :: Behaviour s a
         -> Event s (Behaviour s a)
         -> Behaviour s a
switchEB b ebx = switchEB' b (ForallBehaviour <$> ebx)


















data Button

buttonPressE :: Button -> Event s ()
buttonPressE = undefined

button1 :: Button
button1 = undefined



data Event s a
data Behaviour s a

instance Functor (Event s) where
  fmap _ _ = undefined

instance Functor (Behaviour s) where
  fmap _ _ = undefined

instance Applicative (Behaviour s) where
  pure  = pureB
  (<*>) = applyB

neverE :: Event s a
neverE = undefined

mergeE :: Event s a -> Event s a -> Event s a
mergeE = undefined


pureB :: a -> Behaviour s a
pureB = undefined

applyB :: Behaviour s (a -> b) -> Behaviour s a -> Behaviour s b
applyB = undefined

applyE :: Behaviour s (a -> b) -> Event s a -> Event s b
applyE = undefined


mapFilterE :: (a -> Maybe b) -> Event s a -> Event s b
mapFilterE = undefined

scanE :: (a -> b -> a) -> a -> Event s b -> Event s a
scanE = undefined

holdB :: a -> Event s a -> Behaviour s a
holdB = undefined


toggleB :: Event s () -> Behaviour s Bool
toggleB e = holdB False $ scanE (\b () -> not b) False e

main :: IO ()
main = return ()
