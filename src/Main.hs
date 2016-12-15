module Main where
import Data.Bool

--       (t0,click),      (t1,click),       ...
-- False,           True,            False, ...
toggle1 :: Behaviour Bool
toggle1 = toggleB (buttonPressE button1)

toggle2 :: Behaviour Bool
toggle2 = toggleB (buttonPressE button2)

toggle3 :: Behaviour Bool
toggle3 = toggleB (buttonPressE button3)


result :: Behaviour Bool
result = switchBB (bool toggle1 toggle2 <$> toggle3)

result' :: Behaviour Bool
result' = bool <$> toggle1 <*> toggle2 <*> toggle3











data Button = Button
  { buttonGUI    :: Behaviour GUI
  , buttonPressE :: Event ()
  }

button :: String -> Rect -> Button
button = undefined

button1 :: Button
button1 = undefined

button2 :: Button
button2 = undefined

button3 :: Button
button3 = undefined


data Page a = Page
  { pageGUI     :: Behaviour GUI
  , configuredB :: Behaviour a
  }


data Wizard a
  = Done a
  | More (Page (Wizard a))

runWizard :: Wizard a -> Page a
runWizard = undefined

nextButton :: Button
nextButton = button "Next" rect

lastPage :: a -> Page a
lastPage x = Page (buttonGUI $ button "Finish" rect) (pure x)


type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickOcc = LeftClick Coord | RightClick Coord
data KeyboardOcc = KeyDown Char | KeyUp Char
data GUI = ButtonGUI Label Size | Window [(Coord, GUI)]

normalButton :: GUI
normalButton = undefined

highlightedButton :: GUI
highlightedButton = undefined


data Rect

isInside :: Coord -> Rect -> Bool
isInside = undefined

rect :: Rect
rect = undefined


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

switchBB :: Behaviour (Behaviour a) -> Behaviour a
switchBB = undefined


toggleB :: Event () -> Behaviour Bool
toggleB e = holdB False
          $ scanE (\b () -> not b) False e

main :: IO ()
main = return ()
