module Main where
import Data.Monoid

data Wizard a
  = Done a
  | More (Page (Wizard a))

runWizard :: Wizard a -> Page a
runWizard (Done x)    = lastPage x
runWizard (More page) = Page
    { pageGUI     = currentPageGUI page <> buttonGUI nextButton
    , configuredB = nextNextNextB page
    }
  where
    nextButton :: Button
    nextButton = button "Next" rect
    
    currentPageGUI :: Page (Wizard a) -> Behaviour GUI
    currentPageGUI = undefined
    
    nextNextNextB :: Page (Wizard a) -> Behaviour a
    nextNextNextB = undefined

lastPage :: a -> Page a
lastPage x = Page (buttonGUI $ button "Finish" rect) (pure x)















data Button = Button
  { buttonGUI    :: Behaviour GUI
  , buttonPressE :: Event ()
  }

button :: String -> Rect -> Button
button = undefined


data Page a = Page
  { pageGUI     :: Behaviour GUI
  , configuredB :: Behaviour a
  }


type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickOcc = LeftClick Coord | RightClick Coord
data KeyboardOcc = KeyDown Char | KeyUp Char
data GUI = ButtonGUI Label Size | Window [(Coord, GUI)]

instance Monoid GUI where
  mempty  = Window []
  mappend (Window []) g2 = g2
  mappend g1 (Window []) = g1
  mappend g1 g2 = Window (toList g1 ++ toList g2)
    where
      toList (Window xs) = xs
      toList x           = [(Pos 0 0, x)]

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

instance Monoid a => Monoid (Behaviour a) where
  mempty = pure mempty
  mappend b1 b2 = mappend <$> b1 <*> b2

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


toggleB :: Event () -> Behaviour Bool
toggleB e = holdB False
          $ scanE (\b () -> not b) False e

main :: IO ()
main = return ()
