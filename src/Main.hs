module Main where

scanE :: (a -> b -> a) -> a -> Event b -> Event a
scanE = undefined






goToPageE :: Event Int
goToPageE = scanE (\page () -> page + 1)
                  1
                  nextPageE

currentPageB :: Behaviour Int
currentPageB = undefined





















data Rect = Rect Coord Size

is_inside :: Coord -> Rect -> Bool
is_inside (Pos x y) (Rect (Pos x0 y0) (Size w h))
    = x >= x0 && x < x0 + w
   && y >= y0 && y < y0 + h

buttonRect :: Rect
buttonRect = Rect (Pos 200 200) (Size 80 30)

nextButtonRect :: Rect
nextButtonRect = Rect (Pos 400 300) (Size 80 30)







data Event a
data Behaviour a

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


type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickOcc = LeftClick Coord | RightClick Coord
data KeyboardOcc = KeyDown Char | KeyUp Char
data GUI = Button Label Size | Window [(Coord, GUI)]

mousePositionB :: Behaviour Coord
mousePositionB = undefined

mouseClickE :: Event ClickOcc
mouseClickE = undefined

nextPageE :: Event ()
nextPageE = mapFilterE f mouseClickE
  where
    f :: ClickOcc -> Maybe ()
    f (LeftClick coord) | coord `is_inside` nextButtonRect
                        = Just ()
    f _                 = Nothing


main :: IO ()
main = return ()
