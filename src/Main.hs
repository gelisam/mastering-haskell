module Main where

type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickEvent = LeftClick Coord | RightClick Coord

data GUI = Button Label Size | Window [(Coord, GUI)]




wizard :: [ClickEvent] -> [GUI]
wizard = undefined
















main :: IO ()
main = putStrLn "Welcome to the course!"
