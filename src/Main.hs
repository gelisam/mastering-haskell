module Main where



--                    |
--       (t0,click),      (t1,click),       ...
-- False,           True,            False, ...
toggle1 :: Behaviour Bool
toggle1 = toggleB (buttonPressE button1)

switchBB :: Behaviour (Behaviour a) -> Behaviour a
switchBB = undefined
















data Button

buttonPressE :: Button -> Event ()
buttonPressE = undefined

button1 :: Button
button1 = undefined



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

scanE :: (a -> b -> a) -> a -> Event b -> Event a
scanE = undefined

holdB :: a -> Event a -> Behaviour a
holdB = undefined


toggleB :: Event () -> Behaviour Bool
toggleB e = holdB False
          $ scanE (\b () -> not b) False e

main :: IO ()
main = return ()
