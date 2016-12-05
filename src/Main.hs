module Main where

type Label = String
data Coord = Pos Int Int
data Size = Size Int Int

data ClickOcc = LeftClick Coord | RightClick Coord
data KeyboardOcc = KeyDown Char | KeyUp Char
data GUI = Button Label Size | Window [(Coord, GUI)]

type Time = Double
type Event a = [(Time, a)]

wizard :: Event ClickOcc -> Event KeyboardOcc -> Event GUI
wizard = undefined

merge :: Event a -> Event a -> Event a
merge []         xs'           = xs'
merge xs         []            = xs
merge ((t,x):xs) ((t',x'):xs') | t <= t'   = (t,x)
                                           : merge xs ((t',x'):xs')
                               | otherwise = (t',x')
                                           : merge ((t,x):xs) xs'














main :: IO ()
main = putStrLn "Welcome to the course!"
