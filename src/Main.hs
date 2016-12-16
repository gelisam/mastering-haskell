{-# LANGUAGE RankNTypes #-}
module Main where

data Reactive a

--                    |
--       (t0,click),      (t1,click),       ...
-- False,           True,            False, ...
toggle1 :: Reactive (Behaviour Bool)
toggle1 = toggleB (buttonPressE button1)

switchEB' :: Behaviour a
          -> Event (Reactive (Behaviour a))
          -> Behaviour a
switchEB' = undefined

switchEB :: Behaviour a
         -> Event (Behaviour a) -> Behaviour a
switchEB b ebx = switchEB' b (return <$> ebx)


















data Button

buttonPressE :: Button -> Event ()
buttonPressE = undefined

button1 :: Button
button1 = undefined



instance Functor Reactive where
  fmap = undefined

instance Applicative Reactive where
  pure = undefined
  (<*>) = undefined

instance Monad Reactive where
  (>>=) = undefined


data Event a
data Behaviour a

instance Functor Event where
  fmap _ _ = undefined

instance Functor Behaviour where
  fmap _ _ = undefined

instance Applicative Behaviour where
  pure  = pureB
  (<*>) = applyB

neverE :: Event a
neverE = undefined

mergeE :: Event a -> Event a -> Event a
mergeE = undefined


pureB :: a -> Behaviour a
pureB = undefined

applyB :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
applyB = undefined

applyE :: Behaviour (a -> b) -> Event a -> Event b
applyE = undefined


mapFilterE :: (a -> Maybe b) -> Event a -> Event b
mapFilterE = undefined

scanE :: (a -> b -> a) -> a -> Event b -> Reactive (Event a)
scanE = undefined

holdB :: a -> Event a -> Reactive (Behaviour a)
holdB = undefined


toggleB :: Event () -> Reactive (Behaviour Bool)
toggleB e = do
  e' <- scanE (\b () -> not b) False e
  holdB False e'

main :: IO ()
main = return ()
