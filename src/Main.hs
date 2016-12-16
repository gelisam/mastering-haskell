module Main where



--                    |
--       (t0,click),      (t1,click),       ...
-- False,           True,            False, ...
toggle1 :: Signal Bool
toggle1 = toggleS (buttonPressS button1)

switchSS :: Signal (Signal a) -> Signal a
switchSS = undefined
















data Button

buttonPressS :: Button -> Signal ()
buttonPressS = undefined

button1 :: Button
button1 = undefined



data Signal a

instance Functor Signal where
  fmap _ _ = undefined

instance Applicative Signal where
  pure  = pureS
  (<*>) = applyS

mergeS :: Signal a -> Signal a -> Signal a
mergeS = undefined


pureS :: a -> Signal a
pureS = undefined

applyS :: Signal (a -> b) -> Signal a -> Signal b
applyS = undefined


scanS :: (a -> b -> a) -> a -> Signal b -> Signal a
scanS = undefined

holdS :: a -> Signal a -> Signal a
holdS = undefined


toggleS :: Signal () -> Signal Bool
toggleS e = holdS False
          $ scanS (\b () -> not b) False e

main :: IO ()
main = return ()
