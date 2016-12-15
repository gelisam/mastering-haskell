module Main where


data Page a = Page
  { pageGUI     :: Behaviour GUI
  , configuredB :: Behaviour a
  }

chooseBool :: Page Bool
chooseBool = Page
    { pageGUI     = buttonGUI toggleButton
    , configuredB = toggleB (buttonPressE toggleButton)
    }
  where
    toggleButton :: Button
    toggleButton = button "Toggle" rect

toggleB :: Event () -> Behaviour Bool
toggleB e = holdB False
          $ scanE (\b () -> not b) False e













data Button = Button
  { buttonGUI    :: Behaviour GUI
  , buttonPressE :: Event ()
  }

button :: String -> Rect -> Button
button = undefined


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


mapFilterE :: (a -> Maybe b) -> Event a -> Event b
mapFilterE = undefined

scanE :: (a -> b -> a) -> a -> Event b -> Event a
scanE = undefined

holdB :: a -> Event a -> Behaviour a
holdB = undefined


main :: IO ()
main = return ()
