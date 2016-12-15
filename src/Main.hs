module Main where
import Data.Bool

data Button = Button
  { buttonGUI    :: Behaviour GUI
  , buttonPressE :: Event ()
  }

button :: String -> Rect -> Button
button label rect = Button
    { buttonGUI    = bool normalGUI highlightedGUI <$> isHoveringB
    , buttonPressE = mapFilterE f mouseClickE
    }
  where
    isHoveringB :: Behaviour Bool
    isHoveringB = isInside <$> mousePositionB <*> pure rect
    
    f :: ClickOcc -> Maybe ()
    f (LeftClick coord) | coord `isInside` rect = Just ()
    f _                 = Nothing

    normalGUI, highlightedGUI :: GUI
    (normalGUI, highlightedGUI) = undefined label


















type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickOcc = LeftClick Coord | RightClick Coord
data KeyboardOcc = KeyDown Char | KeyUp Char
data GUI = ButtonGUI Label Size | Window [(Coord, GUI)]

instance Monoid GUI where
  mempty = Window []
  mappend g1 g2 = Window (toList g1 ++ toList g2)
    where
      toList (Window xs) = xs
      toList g           = [(Pos 0 0, g)]


data Rect

isInside :: Coord -> Rect -> Bool
isInside = undefined

rect1 :: Rect
rect1 = undefined

rect2 :: Rect
rect2 = undefined

rect3 :: Rect
rect3 = undefined


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


mouseClickE :: Event ClickOcc
mouseClickE = undefined

mousePositionB :: Behaviour Coord
mousePositionB = undefined


main :: IO ()
main = return ()
