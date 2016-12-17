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

zoom3x :: Image -> Image
zoom3x = thrice . map thrice
  where thrice = concatMap (\x -> [x,x,x])

draw :: Image -> IO ()
draw image = mapM_ putStrLn image

main :: IO ()
main = draw (zoom3x circle)
