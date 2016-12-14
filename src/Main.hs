module Main where

instance Applicative Behaviour where
  pure  = pureB
  (<*>) = applyB

mousePositionB :: Behaviour Coord
mousePositionB = undefined


isHoveringB :: Behaviour Bool
isHoveringB = (&&) <$> (isInside <$> mousePositionB
                                 <*> pure buttonRect)
                   <*> ((== 4) <$> currentPageB)

currentPageB :: Behaviour Int
currentPageB = undefined




















data Rect = Rect Coord Size

isInside :: Coord -> Rect -> Bool
isInside (Pos x y) (Rect (Pos x0 y0) (Size w h))
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


neverE :: Event a
neverE = undefined

mergeE :: Event a -> Event a -> Event a
mergeE = undefined


pureB :: a -> Behaviour a
pureB = undefined

applyB :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
applyB = undefined



type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickOcc = LeftClick Coord | RightClick Coord
data KeyboardOcc = KeyDown Char | KeyUp Char
data GUI = Button Label Size | Window [(Coord, GUI)]

mouseClickE :: Event ClickOcc
mouseClickE = undefined


main :: IO ()
main = return ()
