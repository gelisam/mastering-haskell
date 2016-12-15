module Main where


isHoveringB :: Behaviour Bool
isHoveringB = (&&) <$> (isInside <$> mousePositionB
                                 <*> pure buttonRect)
                   <*> ((== 4) <$> currentPageB)



















data Coord
data Rect

isInside :: Coord -> Rect -> Bool
isInside = undefined

buttonRect :: Rect
buttonRect = undefined



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


mousePositionB :: Behaviour Coord
mousePositionB = undefined

nextPageE :: Event ()
nextPageE = undefined

currentPageB :: Behaviour Int
currentPageB = undefined


main :: IO ()
main = return ()
