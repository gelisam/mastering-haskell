module Main where

type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickOcc = LeftClick Coord | RightClick Coord
data KeyboardOcc = KeyDown Char | KeyUp Char
data GUI = Button Label Size | Window [(Coord, GUI)]

type Time = Double
type Event a = (a -> IO ()) -> IO ()

wizard :: Event ClickOcc -> Event KeyboardOcc -> Event GUI
wizard = undefined

merge :: Event a -> Event a -> Event a
merge register register' = \callback -> do
  register callback
  register' callback













main :: IO ()
main = putStrLn "Welcome to the course!"
