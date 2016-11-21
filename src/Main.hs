module Main where

data ConsoleF a
  = GetLine         (String -> a)
  | PutStrLn String (()     -> a)





getLineLength :: ConsoleF Int
getLineLength =
    GetLine $ \line ->
    length line

echo :: ConsoleF (ConsoleF ())
echo =
    GetLine $ \line ->
    PutStrLn line $ \() ->
    ()



































































































main :: IO ()
main = return ()
