module Main where
type Image = [String]

circle :: Image
circle = [ ".....******....."
         , "..************.."
         , ".**************."
         , "****************"
         , "****************"
         , "****************"
         , ".**************."
         , "..************.."
         , ".....******....."
         ]





draw :: Image -> IO ()
draw image = mapM_ putStrLn image

main :: IO ()
main = draw circle
