module Main where

type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickEvent = LeftClick Coord | RightClick Coord
data KeyboardEvent = KeyDown Char | KeyUp Char
data GUI = Button Label Size | Window [(Coord, GUI)]

type Time = Double
type Event a = [(Time, a)]

wizard :: Event ClickEvent -> Event KeyboardEvent -> [GUI]
wizard = undefined














main :: IO ()
main = putStrLn "Welcome to the course!"
