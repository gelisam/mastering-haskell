{-# LANGUAGE ImpredicativeTypes, RankNTypes #-}
module Main where

data Reactive s a

--                    |
--       (t0,click),      (t1,click),       ...
-- False,           True,            False, ...
toggle1 :: Reactive s (Behaviour s Bool)
toggle1 = toggleB (buttonPressE button1)

switchEB' :: Behaviour s a
          -> Event s (forall t. Reactive t (Behaviour t a))
          -> Behaviour s a
switchEB' = undefined

switchEB :: Behaviour s a
         -> Event s (Behaviour s a)
         -> Behaviour s a
switchEB b ebx = switchEB' b (return <$> ebx)


















data Button

buttonPressE :: Button -> Event s ()
buttonPressE = undefined

button1 :: Button
button1 = undefined



instance Functor (Reactive s) where
  fmap = undefined

instance Applicative (Reactive s) where
  pure = undefined
  (<*>) = undefined

instance Monad (Reactive s) where
  (>>=) = undefined


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

scanE :: (a -> b -> a) -> a -> Event s b -> Reactive s (Event s a)
scanE = undefined

holdB :: a -> Event s a -> Reactive s (Behaviour s a)
holdB = undefined


toggleB :: Event s () -> Reactive s (Behaviour s Bool)
toggleB e = do
  e' <- scanE (\b () -> not b) False e
  holdB False e'

main :: IO ()
main = return ()
